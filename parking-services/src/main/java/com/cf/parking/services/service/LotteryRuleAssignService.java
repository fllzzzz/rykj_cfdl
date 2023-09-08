package com.cf.parking.services.service;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryRuleAssignMapper;
import com.cf.parking.dao.po.LotteryApplyRecordPO;
import com.cf.parking.dao.po.LotteryRuleAssignPO;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.services.enums.RuleAssignTypeEnum;

import lombok.extern.slf4j.Slf4j;



@Service
@Slf4j
public class LotteryRuleAssignService extends ServiceImpl<LotteryRuleAssignMapper, LotteryRuleAssignPO> implements IService<LotteryRuleAssignPO>{

	
	@Resource
	private LotteryRuleAssignMapper ruleAssignMapper;

	@Resource
    private EmployeeService employeeService;
	
	
	
	/**
	 * 根据类型和code查询配置的
	 * @param jobNumberList
	 * @param state
	 */
	public List<LotteryRuleAssignPO> queryRuleAssignListByJobNumber(List<String> codeList, String type) {
		
		return ruleAssignMapper.selectList(new LambdaQueryWrapper<LotteryRuleAssignPO>()
					.eq(LotteryRuleAssignPO::getType, type)
					.in(LotteryRuleAssignPO::getCode, codeList)
				);
	}
	
	
	/**
	 * 根据配置的部门/人员规则对申请人员做过滤
	 * 人员的优先级最高，部门次之，都没有配的话就用员工个人中心的停车场
	 * @param parkLot
	 * @param applyList
	 * @return
	 */
	public List<LotteryApplyRecordPO> dealApplyByRule(ParkingLotPO parkLot,List<LotteryApplyRecordPO> applyList){
		/**
		 * 对人员/部门停车场规则设置这块的逻辑是:
		 * 1.根据上面查询到的报名人员工号，去查询对应的人员配置规则。
		 * 2.然后按照人员---停车场维度进行分组，如果当前的人员配置的停车场不包含摇号的停车场，就剔除该摇号人员
		 * 3.根据报名人员的部门去查询对应的部门配置规则
		 * 4.然后按照部门---停车场维度进行分组，如果当前的部门配置的停车场不包含摇号的停车场，就剔除该部门的摇号人员
		 * 5.留下的最终结果即为摇号人员
		 */
		
		//获取该车库的报名人员信息
		List<LotteryApplyRecordPO> parkApplyList = applyList.stream().filter(item-> item.getParkingLotCode().equals(parkLot.getRegionCode())).collect(Collectors.toList());
		//获取该车库报名人员的工号
		List<String> jobNumberList = applyList.stream().filter(item-> item.getParkingLotCode().equals(parkLot.getRegionCode())).map(item -> item.getJobNumber()).collect(Collectors.toList());
		log.info("获取到报名停车场：{}的人员工号{}",parkLot.getRegionCode(),JSON.toJSONString(jobNumberList));
		
		//获取配置人员数据
		List<LotteryRuleAssignPO> empList = queryRuleAssignListByJobNumber(jobNumberList,RuleAssignTypeEnum.EMPLOYEE.getState());
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
		List<LotteryRuleAssignPO> deptList = queryRuleAssignListByJobNumber(deptCodeList,RuleAssignTypeEnum.DEPARMENT.getState());
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
		return parkApplyList;
	}

	
}
