package com.cf.parking.services.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import javax.annotation.Resource;

import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import com.alibaba.fastjson.JSON;
import com.cf.parking.dao.po.LotteryApplyRecordPO;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.support.bean.IdWorker;
import com.cf.support.exception.BusinessException;
import lombok.extern.slf4j.Slf4j;




@Service
@Slf4j
public class LotteryDealService {

	@Resource
	private IdWorker idWorker;
	
	/**
	 * 摇号
	 * @param batch
	 * @param lottery
	 * @param round
	 */

	public void doLottery(LotteryBatchPO batch, LotteryResultPO lottery, ParkingLotPO parkLot,
			List<LotteryApplyRecordPO> parkApplyList) {
		log.info("摇号入参对象：batch:{},lottery:{},parking:{},employee:{}",JSON.toJSONString(batch),JSON.toJSONString(lottery),JSON.toJSONString(parkLot),JSON.toJSONString(parkApplyList));
		if (CollectionUtils.isEmpty(parkApplyList)) {
			throw new BusinessException("没有符合摇号条件的人员");
		}
		
		//存放中签结果
		List<LotteryResultDetailPO> result = new ArrayList<>();
		
		//车位比摇号人数量多，全部中签
		if (parkLot.getAmount() >= parkApplyList.size()) {
			
		}
		
		//存放中签序号
		Set<Integer> indexSet = generateLotteryIndex(parkLot.getAmount(),parkApplyList.size());
		
		
		
		
	}

	private LotteryResultDetailPO generateLotteryResult(LotteryApplyRecordPO apply) {
		LotteryResultDetailPO detail = new LotteryResultDetailPO();
		detail.setCreateTm(new Date()).setId(idWorker.nextId()).setParkingLotId(null);
		
		return detail;
	}
	
	/**
	 * 随机抽取报名摇号序号
	 * @param amount
	 * @param total
	 * @return
	 */
	private static Set<Integer> generateLotteryIndex(long amount, int total) {
		Random random = new Random();
		Set<Integer> result = new HashSet<>();
		while(result.size() != amount) {
			result.add(random.nextInt(total+1));
		}
		return result;
	}


}
