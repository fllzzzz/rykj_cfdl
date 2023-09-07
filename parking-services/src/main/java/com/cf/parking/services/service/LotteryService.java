package com.cf.parking.services.service;

import org.springframework.stereotype.Service;

import com.alibaba.fastjson.JSON;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.dao.po.LotteryRuleRoundPO;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class LotteryService {

	/**
	 * 摇号
	 * @param batch
	 * @param lottery
	 * @param round
	 */
	public void doLottery(LotteryBatchPO batch, LotteryResultPO lottery, LotteryRuleRoundPO round) {
		
		log.info("摇号入参对象：batch:{},lottery:{},round:{}",JSON.toJSONString(batch),JSON.toJSONString(lottery),JSON.toJSONString(round));
		
		
		
	}

}
