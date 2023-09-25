package com.cf.parking.services.service;

import java.util.ArrayList;
import java.util.Collections;
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
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.dao.po.LotteryRuleAssignPO;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.services.enums.LotteryResultStateEnum;
import com.cf.parking.services.enums.RuleAssignTypeEnum;

import lombok.extern.slf4j.Slf4j;



@Service
@Slf4j
public class LotteryRuleAssignService extends ServiceImpl<LotteryRuleAssignMapper, LotteryRuleAssignPO> implements IService<LotteryRuleAssignPO>{

	
	@Resource
	private LotteryRuleAssignMapper ruleAssignMapper;

	@Resource
    private EmployeeService employeeService;
	
	@Resource
	private LotteryResultService lotteryResultService;
	
	
	
	/**
	 * 根据类型和code查询配置的
	 * @param jobNumberList
	 * @param state
	 */
	public List<LotteryRuleAssignPO> queryRuleAssignListByJobNumber(List<String> codeList, String type) {
		
		return CollectionUtils.isEmpty(codeList) ? Collections.emptyList() : ruleAssignMapper.selectList(new LambdaQueryWrapper<LotteryRuleAssignPO>()
					.eq(LotteryRuleAssignPO::getType, type)
					.in(LotteryRuleAssignPO::getCode, codeList)
				);
	}
	
	
	/**
	 * 根据轮次查询部门/人员设置
	 * @param roundId
	 * @return
	 */
	public List<LotteryRuleAssignPO> queryRuleListByRoundId(Long roundId){
		return ruleAssignMapper.selectList(new LambdaQueryWrapper<LotteryRuleAssignPO>()
					.eq(LotteryRuleAssignPO::getRoundId, roundId)
				);
	}
	
	
	/**
	 * 根据配置的部门/人员规则对申请人员做过滤
	 * @param batchId
	 * @param applyList
	 * @param roundId 轮次Id 
	 * @return
	 */
	public List<LotteryApplyRecordPO> dealApplyByRule(Long batchId,List<LotteryApplyRecordPO> applyList, Long roundId){
		/**
		 * 1.如果某一轮添加了部门/人员设置，那就只能由这些指定的人员来参加这轮摇号；
		 * 2.如果连续几轮都设置了部门/人员配置，且有某些用户能同时匹配到这几轮，那么这些用户也只能参加其中的一轮。
		 * 3.没有设置部门/人员的轮数，本期未中奖的人员均可以参加（包含第二种情况里未中奖人员）。
		 */
		
		//获取部门人员/配置
		List<LotteryRuleAssignPO> ruleList = queryRuleListByRoundId(roundId);
		if (CollectionUtils.isEmpty(ruleList)) {//没有设置就直接返回报名人员
			return applyList;
		}
		
		List<String> ruleJobList = getJobNumFromRuleList(ruleList);
		//取两个集合的交集做摇号人员
		applyList = applyList.stream().filter(apply -> ruleJobList.contains(apply.getJobNumber())).collect(Collectors.toList()); 
		//获取前几轮已配置摇号人员工号
		List<String> excludeList = queryRuleAssignList(batchId,roundId);
		applyList = applyList.stream().filter(apply -> !excludeList.contains(apply.getJobNumber())).collect(Collectors.toList()); 
		return applyList;
	}


	/**
	 * 根据轮次查询已摇号的轮次的人员设置，排除掉轮次Id为roundId的轮次
	 * @param batchId
	 * @param roundId
	 * @return
	 */
	public List<String> queryRuleAssignList(Long batchId, Long roundId) {
		List<LotteryResultPO> resultList = lotteryResultService.selectResultListByBatchId(batchId);
		resultList = resultList.stream().filter(result -> !LotteryResultStateEnum.UNLOTTERY.getState().equals(result.getState()) && result.getRoundId() != roundId  ).collect(Collectors.toList());
		if (CollectionUtils.isEmpty(resultList)) {
			return Collections.emptyList();
		}
		List<Long> roundIdList = resultList.stream().map(result -> result.getRoundId()).collect(Collectors.toList());
		List<LotteryRuleAssignPO> ruleList = ruleAssignMapper.selectList(new LambdaQueryWrapper<LotteryRuleAssignPO>()
				.in(LotteryRuleAssignPO::getRoundId, roundIdList)
			);
		return null;
	}
	
	/**
	 * 通过配置获取人员工号
	 * @param ruleList
	 * @return
	 */
	private List<String> getJobNumFromRuleList(List<LotteryRuleAssignPO> ruleList){
		if(CollectionUtils.isEmpty(ruleList)) {
			return Collections.emptyList();
		}
		//人员工号集合
		List<String> ruleJobList = new ArrayList<>();
		//部门代码集合
		List<String> deptCodeList = new ArrayList<>();
				
		ruleList.forEach(rule -> {
			if (RuleAssignTypeEnum.EMPLOYEE.getState().equals(rule.getType())) {
				ruleJobList.add(rule.getCode());
			} else if (RuleAssignTypeEnum.DEPARMENT.getState().equals(rule.getType())) {
				deptCodeList.add(rule.getCode());
			}
		});
		//获取部门下的人员
		ruleJobList.addAll(employeeService.queryEmployeeListByDept(deptCodeList));
		deptCodeList.clear();
		return ruleJobList;
	}

}
