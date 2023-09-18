package com.cf.parking.services.service;

import javax.annotation.Resource;

import org.springframework.stereotype.Service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryRuleRoundMapper;
import com.cf.parking.dao.po.LotteryRuleRoundPO;
import com.cf.parking.services.enums.EnableStateEnum;



@Service
public class LotteryRuleRoundService extends ServiceImpl<LotteryRuleRoundMapper, LotteryRuleRoundPO> implements IService<LotteryRuleRoundPO>{

	@Resource
	private LotteryRuleRoundMapper lotteryRuleRoundMapper;
	
	
	public LotteryRuleRoundPO queryDefaultRound() {
		
		return lotteryRuleRoundMapper.selectOne(new LambdaQueryWrapper<LotteryRuleRoundPO>()
					.eq(LotteryRuleRoundPO::getState, EnableStateEnum.ENABLE.getState())
					.orderByAsc(LotteryRuleRoundPO::getCreateTm)
					.last(" limit 1 ")
				);
	}

	
}
