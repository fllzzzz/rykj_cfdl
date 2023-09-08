package com.cf.parking.services.facade.impl;


import com.alibaba.fastjson.JSON;
import com.cf.parking.dao.mapper.LotteryResultMapper;
import com.cf.parking.dao.po.LotteryApplyRecordPO;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.dao.po.LotteryRuleAssignPO;
import com.cf.parking.dao.po.LotteryRuleRoundPO;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.facade.facade.LotteryResultFacade;
import com.cf.parking.services.enums.EnableStateEnum;
import com.cf.parking.services.enums.RuleAssignTypeEnum;
import com.cf.parking.services.service.DepartmentService;
import com.cf.parking.services.service.EmployeeService;
import com.cf.parking.services.service.LotteryApplyRecordService;
import com.cf.parking.services.service.LotteryBatchService;
import com.cf.parking.services.service.LotteryBlackListService;
import com.cf.parking.services.service.LotteryDealService;
import com.cf.parking.services.service.LotteryRuleAssignService;
import com.cf.parking.services.service.LotteryRuleRoundService;
import com.cf.parking.services.service.ParkingLotService;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.exception.BusinessException;
import lombok.extern.slf4j.Slf4j;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

/**
 * 摇号结果Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Service
@Slf4j
public class LotteryResultFacadeImpl implements LotteryResultFacade
{
    @Resource
    private LotteryResultMapper lotteryResultMapper;

    @Resource
    private LotteryRuleRoundService lotteryRuleRoundService;
    
    @Resource
    private LotteryBatchService lotteryBatchService;
    
    @Resource
    private LotteryDealService lotteryDealService;
    
    @Resource
    private LotteryRuleAssignService lotteryRuleAssignService;
    
    @Resource
    private ParkingLotService parkingLotService;
    
    @Resource
    private LotteryApplyRecordService lotteryApplyRecordService;
    
    @Resource
    private EmployeeService employeeService;
    
    
    @Resource
    private LotteryBlackListService lotteryBlackListService;
    
    
    @Resource
    private DepartmentService departmentService;
    
    
	@Override
	public void lottery(Long id) {
		
		log.info("开始摇号：{}",id);
		LotteryResultPO lottery = lotteryResultMapper.selectById(id);
		AssertUtil.checkNull(lottery, "数据不存在");
		LotteryRuleRoundPO round = lotteryRuleRoundService.getById(lottery.getRoundId());
		AssertUtil.checkNull(round, "轮次数据不存在");
		AssertUtil.checkTrue(EnableStateEnum.ENABLE.getState().equals(round.getState()),"摇号轮次未启用");
		LotteryBatchPO batch = lotteryBatchService.getById(lottery.getBatchId());
		AssertUtil.checkNull(batch, "批次数据不存在");
		//获取停车场信息
		String parklotCode = round.getParkingLotCode();
		List<String> parkingList = Arrays.asList(parklotCode.split(","));
		List<ParkingLotPO> parkingLotList = parkingLotService.selectParkingLotByCodes(parkingList);
		log.info("初步获取到摇号停车场信息：{}",JSON.toJSONString(parkingLotList));
		AssertUtil.checkTrue(!CollectionUtils.isEmpty(parkingLotList), "停车场信息不存在，请检查设置");
		parkingLotList = parkingLotList.stream().filter(item -> EnableStateEnum.ENABLE.getState().equals(item.getType())).collect(Collectors.toList());
		AssertUtil.checkTrue(!CollectionUtils.isEmpty(parkingLotList), "停车场的配置为不可参加摇号，请检查设置");
		//获取报名的人员,要符合个人中心的车库在这次摇号的车库中这个条件
		List<LotteryApplyRecordPO> applyList = lotteryApplyRecordService.queryLotteryApplyList(lottery.getBatchId(),parkingList);
		log.info("获取到报名摇号的人员信息：{}",JSON.toJSONString(applyList));
		if (CollectionUtils.isEmpty(applyList)) {
			log.info("无报名摇号人员，程序退出");
			throw new BusinessException("没有人员报名摇号");
		}
		//获取全部报名人员的工号
		List<String> jobNumberList = applyList.stream().map(item -> item.getJobNumber()).collect(Collectors.toList());
		//根据工号获取在职员工信息
		List<String> employeeJobNumList = employeeService.queryEmployeeListByJobNum(jobNumberList);
		//获取黑名单用户
		List<String> blackJobNumList = lotteryBlackListService.queryBlackList();
		log.info("黑名单信息：{}",blackJobNumList);
		//过滤掉离职人员和黑名单人员
		applyList = applyList.stream().filter(item -> (employeeJobNumList.contains(item.getJobNumber()) && !blackJobNumList.contains(item.getJobNumber()) ) ).collect(Collectors.toList());
		log.info("过滤掉离职和黑名单后的报名摇号的人员信息：{}",JSON.toJSONString(applyList));
		//释放数据
		jobNumberList.clear();
		blackJobNumList.clear();
		employeeJobNumList.clear();
		
		List<LotteryApplyRecordPO> parkApplyList = new ArrayList<>();
		for(ParkingLotPO parkLot : parkingLotList) {
			/**
			 * 对人员/部门停车场规则设置这块的逻辑是:
			 * 1.根据上面查询到的报名人员工号，去查询对应的人员配置规则。
			 * 2.然后按照人员---停车场维度进行分组，如果当前的人员配置的停车场不包含摇号的停车场，就剔除该摇号人员
			 * 3.根据报名人员的部门去查询对应的部门配置规则
			 * 4.然后按照部门---停车场维度进行分组，如果当前的部门配置的停车场不包含摇号的停车场，就剔除该部门的摇号人员
			 * 5.留下的最终结果即为摇号人员
			 */
			//获取该车库的报名人员信息
			parkApplyList = applyList.stream().filter(item-> item.getParkingLotCode().equals(parkLot.getRegionCode())).collect(Collectors.toList());
			//获取该车库报名人员的工号
			jobNumberList = applyList.stream().filter(item-> item.getParkingLotCode().equals(parkLot.getRegionCode())).map(item -> item.getJobNumber()).collect(Collectors.toList());
			log.info("获取到报名停车场：{}的人员工号{}",parkLot.getRegionCode(),JSON.toJSONString(jobNumberList));
			
			//获取配置人员数据
			List<LotteryRuleAssignPO> empList = lotteryRuleAssignService.queryRuleAssignListByJobNumber(jobNumberList,RuleAssignTypeEnum.EMPLOYEE.getState());
			if(!CollectionUtils.isEmpty(empList)) {
				//把数据映射成工号-->停车集合的形式
				Map<String,List<String>> userParking = empList.stream().collect(Collectors.groupingBy(LotteryRuleAssignPO::getCode,Collectors.mapping(LotteryRuleAssignPO::getParkingLotCode, Collectors.toList())));
				//获取到待剔除人员工号
				List<String> deleteJobNumList = userParking.entrySet().stream().filter(item -> !item.getValue().contains(parkLot.getRegionCode())).map(Map.Entry::getKey).collect(Collectors.toList());
				log.info("获取到因未设置车库={}而需要剔除的人员工号={}",parkLot.getRegionCode(),deleteJobNumList);
				parkApplyList = parkApplyList.stream().filter(item -> !deleteJobNumList.contains(item.getParkingLotCode())).collect(Collectors.toList());
				deleteJobNumList.clear();
				userParking.clear();
			}
			
			//处理部门配置
			
			//根据包名人员工号获取对应的部门id
			List<String> deptCodeList = employeeService.queryDeptListByJobNum(jobNumberList);
			//获取配置部门的数据
			List<LotteryRuleAssignPO> deptList = lotteryRuleAssignService.queryRuleAssignListByJobNumber(deptCodeList,RuleAssignTypeEnum.DEPARMENT.getState());
			if(!CollectionUtils.isEmpty(deptList)) {
				//把数据映射成部门代号-->停车集合的形式
				Map<String,List<String>> departParking = deptList.stream().collect(Collectors.groupingBy(LotteryRuleAssignPO::getCode,Collectors.mapping(LotteryRuleAssignPO::getParkingLotCode, Collectors.toList())));
				//获取到待剔除部门工号
				List<String> deleteDeptList = departParking.entrySet().stream().filter(item -> !item.getValue().contains(parkLot.getRegionCode())).map(Map.Entry::getKey).collect(Collectors.toList());
				log.info("获取到因未设置车库={}而需要剔除的部门工号={}",parkLot.getRegionCode(),deleteDeptList);
				List<String> deptEmplyeeList = employeeService.queryEmployeeListByDept(deleteDeptList);
				parkApplyList = parkApplyList.stream().filter(item -> !deptEmplyeeList.contains(item.getParkingLotCode())).collect(Collectors.toList());
				deleteDeptList.clear();
				departParking.clear();
				deptEmplyeeList.clear();
			}
			
			lotteryDealService.doLottery(batch,lottery,parkLot,parkApplyList);
			parkApplyList.clear();
		}
		
	}

	
    /**
     * 查询摇号结果
     * 
     * @param id 摇号结果主键
     * @return 摇号结果
     */
//    @Override
//    public LotteryResult selectLotteryResultById(Long id)
//    {
//        return lotteryResultMapper.selectLotteryResultById(id);
//    }

    /**
     * 查询摇号结果列表
     * 
     * @param lotteryResult 摇号结果
     * @return 摇号结果
     */
//    @Override
//    public List<LotteryResult> selectLotteryResultList(LotteryResult lotteryResult)
//    {
//        return lotteryResultMapper.selectLotteryResultList(lotteryResult);
//    }

    /**
     * 新增摇号结果
     * 
     * @param lotteryResult 摇号结果
     * @return 结果
     */
//    @Override
//    public int insertLotteryResult(LotteryResult lotteryResult)
//    {
//        return lotteryResultMapper.insertLotteryResult(lotteryResult);
//    }

    /**
     * 修改摇号结果
     * 
     * @param lotteryResult 摇号结果
     * @return 结果
     */
//    @Override
//    public int updateLotteryResult(LotteryResult lotteryResult)
//    {
//        return lotteryResultMapper.updateLotteryResult(lotteryResult);
//    }

    /**
     * 批量删除摇号结果
     * 
     * @param ids 需要删除的摇号结果主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryResultByIds(Long[] ids)
//    {
//        return lotteryResultMapper.deleteLotteryResultByIds(ids);
//    }

    /**
     * 删除摇号结果信息
     * 
     * @param id 摇号结果主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryResultById(Long id)
//    {
//        return lotteryResultMapper.deleteLotteryResultById(id);
//    }
}
