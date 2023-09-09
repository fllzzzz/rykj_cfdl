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
import com.cf.parking.services.enums.LotteryResultStateEnum;
import com.cf.support.bean.IdWorker;
import com.cf.support.exception.BusinessException;
import lombok.extern.slf4j.Slf4j;




@Service
@Slf4j
public class LotteryDealService {

	@Resource
	private IdWorker idWorker;
	
	@Resource
	private LotteryResultDetailService lotteryResultDetailService; 
	
	@Resource
	private LotteryResultService lotteryResultService; 
	
	
	@Resource
	private UserSpaceService userSpaceService;
	
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
			parkApplyList.forEach(apply -> {
				LotteryResultDetailPO detail = generateLotteryResult(apply,parkLot.getRegionCode(),lottery.getId());
				result.add(detail);
			});
		} else {
			//存放中签序号
			Set<Integer> indexSet = generateLotteryIndex(parkLot.getAmount(),parkApplyList.size());
			indexSet.forEach(index -> {
				LotteryResultDetailPO detail = generateLotteryResult(parkApplyList.get(index),parkLot.getRegionCode(),lottery.getId());
				result.add(detail);
			});
		}
		log.info("摇号结果入库：{}",JSON.toJSONString(result));
		lotteryResultDetailService.saveBatch(result);
		lottery.setUpdateTm(new Date()).setState(LotteryResultStateEnum.UNCONFIRM.getState());
		lotteryResultService.updateById(lottery);
	}

	/**
	 * 封装结果明细数据
	 * @param apply
	 * @param parkLotCode
	 * @param resultId
	 * @return
	 */
	private LotteryResultDetailPO generateLotteryResult(LotteryApplyRecordPO apply,String parkLotCode,Long resultId) {
		LotteryResultDetailPO detail = new LotteryResultDetailPO();
		detail.setCreateTm(new Date()).setId(idWorker.nextId()).setParkingLotCode(parkLotCode)
		.setResultId(resultId).setUpdateTm(new Date()).setUserId(apply.getUserId())
		.setUserName(apply.getUserName()).setUserJobNumber(apply.getJobNumber());
		return detail;
	}
	
	/**
	 * 随机抽取报名摇号序号
	 * @param amount 中签数量
	 * @param total  参与数量
	 * @return
	 */
	private static Set<Integer> generateLotteryIndex(long amount, int total) {
		Random random = new Random();
		Set<Integer> result = new HashSet<>();
		while(result.size() != amount) {
			result.add(random.nextInt(total));
		}
		return result;
	}


}
