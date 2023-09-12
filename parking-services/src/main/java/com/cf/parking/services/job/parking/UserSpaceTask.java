package com.cf.parking.services.job.parking;

import java.util.Date;
import javax.annotation.Resource;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.services.job.annotation.TaskLock;
import com.cf.parking.services.service.UserSpaceService;
import cn.hutool.core.date.DateUtil;
import lombok.extern.slf4j.Slf4j;



/**
 * 车位定时任务
 * @author think
 *
 */
@Slf4j
@Component
public class UserSpaceTask {

	@Resource
	private UserSpaceService userSpaceService;
	
	
	
	
	/**
	 * 删除过期车
	 */
	@Scheduled(cron = "0 1 0 * * ? ") //间隔1天
	@TaskLock(key = RedisConstant.PARKING_EXPIRED_LOCK_KEY)
	public void dealExpiredSpace() {
		
		try {
			String time = DateUtil.format(new Date(), "yyyy-MM-dd");
			log.info("删除过期车定时任务：{}",time);
			userSpaceService.deleteExpiredSpace(time);
		} catch (Exception e) {
			log.error("删除过期车定时任务={}",e);
		}
	}
	
	
	/**
	 * 针对一人多库的任务，是在生效期开始当天进行下发闸机
	 */
	@Scheduled(cron = "0 10 0 * * ? ") //间隔1天
	@TaskLock(key = RedisConstant.PARKING_DOWN_LOCK_KEY)
	public void parkingDown() {
		
		try {
			String time = DateUtil.format(new Date(), "yyyy-MM-dd");
			log.info("按照表里的定时器时间进行下发闸机：{}",time);
			userSpaceService.parkingDownOnStartTtime(time);
		} catch (Exception e) {
			log.error("按照表里的定时器时间进行下发闸机失败={}",e);
		}
	}
	
	
	/**
	 * 把车位表里的数据同步到闸机系统
	 */
	@Scheduled(cron = "0/5 * * * * ? ") //间隔5秒
	@TaskLock(key = RedisConstant.PARKING_SYNC_LOCK_KEY)
	public void syncSpace() {
		try {
			userSpaceService.syncSpace();
		} catch (Exception e) {
			log.error("同步车位信息失败：{}",e);
		}
	}
}
