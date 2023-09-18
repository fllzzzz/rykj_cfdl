package com.cf.parking.services.job.parking;

import javax.annotation.Resource;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.services.job.annotation.TaskLock;
import com.cf.parking.services.service.LotteryBatchService;
import cn.hutool.core.date.DateUtil;
import lombok.extern.slf4j.Slf4j;

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
	
	
	@Scheduled(cron = "0 0 6 20 1,3,5,7,9,11 ? ") //奇数月的15号早上6点执行
	@TaskLock(key = RedisConstant.AUTO_CREATE_BATCH)
	public void autoCreateBatch() {
		boolean exist = lotteryBatchService.queryNextMonthBatchExist(DateUtil.format(DateUtil.beginOfMonth( DateUtil.nextMonth()), "yyyy-MM-dd"),
				DateUtil.format(DateUtil.endOfMonth( DateUtil.nextMonth()), "yyyy-MM-dd"));
		
		if (!exist) {
			lotteryBatchService.autoCreateBatch();
		}
	}
	
	
}
