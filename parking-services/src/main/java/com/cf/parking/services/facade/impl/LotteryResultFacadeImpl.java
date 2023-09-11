package com.cf.parking.services.facade.impl;


import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryResultMapper;
import com.cf.parking.dao.po.*;
import com.cf.parking.facade.bo.LotteryResultBO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.dto.LotteryResultDTO;
import com.cf.parking.facade.facade.LotteryResultFacade;
import com.cf.parking.services.enums.EnableStateEnum;
import com.cf.parking.services.service.*;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.exception.BusinessException;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

/**
 * 摇号结果Service业务层处理
 * 
 * @author
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
	private LotteryResultDetailService lotteryResultDetailService;
    
    
    @Resource
    private DepartmentService departmentService;
    
    
    @Transactional(rollbackFor = Exception.class)
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
			parkApplyList = lotteryRuleAssignService.dealApplyByRule(parkLot, applyList);
			lotteryDealService.doLottery(batch,lottery,parkLot,parkApplyList);
			parkApplyList.clear();
		}
		
	}

	/**
	 * 查询摇号结果列表
	 * @param dto
	 * @return
	 */
	@Override
	public PageResponse<LotteryResultBO> getLotteryResultList(LotteryResultDTO dto) {
		Page<LotteryResultPO> page = PageUtils.toPage(dto);

		LambdaQueryWrapper<LotteryResultPO> queryWrapper = new LambdaQueryWrapper<LotteryResultPO>()
				.le(!ObjectUtils.isEmpty(dto.getEndDate()), LotteryResultPO::getBatchNum, dto.getEndDate())
				.ge(!ObjectUtils.isEmpty(dto.getStartDate()) , LotteryResultPO::getBatchNum, dto.getStartDate())
				.like(!ObjectUtils.isEmpty(dto.getRoundId()) , LotteryResultPO::getRoundId, dto.getRoundId())
				.eq(StringUtils.isNotEmpty(dto.getState()), LotteryResultPO::getState, dto.getState())
				.ne(LotteryResultPO::getState,"5")
				.orderByDesc(LotteryResultPO::getBatchNum);

		Page<LotteryResultPO> poPage = lotteryResultMapper.selectPage(page, queryWrapper);
		List<LotteryResultBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryResultBO.class);
		return PageUtils.toResponseList(page,boList);
	}

	/**
	 * 结果归档(当本期所有记录均归档完成后，将该批次的状态更改为已结束)
	 * @param id
	 * @return
	 */
	@Transactional(rollbackFor = Exception.class)
	@Override
	public Integer archive(Long id) {

		//1.更新状态为已归档
		LotteryResultPO po = lotteryResultMapper.selectById(id);
		po.setState("5");
		int result = lotteryResultMapper.updateById(po);

		//2.查看相同期号下是否还有其他未归档的记录。如果没有，将该期摇号批次表状态变为已结束
		List<LotteryResultPO> lotteryResultPOList = lotteryResultMapper.selectList(new LambdaQueryWrapper<LotteryResultPO>()
				.eq(LotteryResultPO::getBatchNum, po.getBatchNum())
				.ne(LotteryResultPO::getState, "5"));

		if (CollectionUtils.isEmpty(lotteryResultPOList)){
			result = lotteryBatchService.archive(po.getBatchId());
		}
		return result;
	}

	/**
	 * 摇号结果分页查询
	 * @param dto
	 * @return
	 */
	@Override
	public PageResponse<LotteryResultDetailBO> lotteryResult(LotteryResultDTO dto) {
		Page<LotteryResultDetailPO> page = PageUtils.toPage(dto);
		return lotteryResultDetailService.selectDetailListByResultId(page, dto.getId());
	}


}
