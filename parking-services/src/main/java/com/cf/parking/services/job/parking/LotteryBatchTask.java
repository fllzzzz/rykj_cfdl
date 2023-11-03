package com.cf.parking.services.job.parking;

import cn.hutool.core.date.DateUtil;
import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.services.constant.ParkingConstants;
import com.cf.parking.services.job.annotation.TaskLock;
import com.cf.parking.services.service.LotteryBatchService;
import com.cf.support.utils.DingAlarmUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * 
 * @author think
 * 
 */
@Slf4j
@Component
public class LotteryBatchTask {

	@Resource
	private LotteryBatchService lotteryBatchService;
	
	
	@Scheduled(cron = "0 0 6 15-20 1,3,5,7,9,11 ? ") //奇数月的15-20号早上6点执行
	@TaskLock(key = RedisConstant.AUTO_CREATE_BATCH)
	public void autoCreateBatch() {
		try {
			boolean exist = lotteryBatchService.queryNextMonthBatchExist(DateUtil.format(DateUtil.beginOfMonth(DateUtil.nextMonth()), ParkingConstants.SHORT_DATE_FORMAT),
					DateUtil.format(DateUtil.endOfMonth(DateUtil.nextMonth()), ParkingConstants.SHORT_DATE_FORMAT));
			log.info("摇号批次是否存在：{}", exist);
			if (!exist) {
				lotteryBatchService.autoCreateBatch();
			}
		}catch (Exception e){
			log.error("autoCreateBatchErr",e);
			DingAlarmUtils.alarmException("autoCreateBatchErr" + e.getMessage());
		}
	}
	
	
}
