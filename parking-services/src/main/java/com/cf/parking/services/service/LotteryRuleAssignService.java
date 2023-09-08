package com.cf.parking.services.service;

import java.util.List;

import javax.annotation.Resource;
import org.springframework.stereotype.Service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryRuleAssignMapper;
import com.cf.parking.dao.po.LotteryRuleAssignPO;



@Service
public class LotteryRuleAssignService extends ServiceImpl<LotteryRuleAssignMapper, LotteryRuleAssignPO> implements IService<LotteryRuleAssignPO>{

	
	@Resource
	private LotteryRuleAssignMapper ruleAssignMapper;

	
	
	
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
	

	
}
