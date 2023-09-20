package com.cf.parking.services.job.parking;

import java.util.Date;
import java.util.List;
import javax.annotation.Resource;

import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import com.alibaba.fastjson.JSON;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.services.constant.ParkingConstants;
import com.cf.parking.services.enums.LotteryResultStateEnum;
import com.cf.parking.services.job.annotation.TaskLock;
import com.cf.parking.services.service.LotteryResultService;
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
	
	@Resource
    private LotteryResultService lotteryResultService;
	
	
	/**
	 * 删除过期车数据
	 */
	@Scheduled(cron = "0 1 0 * * ? ") //间隔1天
	@TaskLock(key = RedisConstant.PARKING_EXPIRED_LOCK_KEY)
	public void dealExpiredSpace() {
		
		try {
			String time = DateUtil.format(new Date(), ParkingConstants.SHORT_DATE_FORMAT);
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
			String time = DateUtil.format(new Date(), ParkingConstants.SHORT_DATE_FORMAT);
			log.info("按照表里的定时器时间进行下发闸机：{}",time);
			userSpaceService.parkingDownOnStartTtime(time);
		} catch (Exception e) {
			log.error("按照表里的定时器时间进行下发闸机失败={}",e);
		}
	}
	
	
	/**
	 * 把车位表里的数据同步到闸机系统
	 */
	@Async
	@Scheduled(cron = "0/5 * * * * ? ") //间隔5秒
	@TaskLock(key = RedisConstant.PARKING_SYNC_LOCK_KEY)
	public void syncSpace() {
		try {
			userSpaceService.syncSpace();
		} catch (Exception e) {
			log.error("同步车位信息失败：{}",e);
		}
	}
	
	
	/**
	 * 判断摇号结果表中的数据是否都已下发闸机成功，是的话就更新结果状态
	 */
	@Scheduled(cron = "0 0 0/1 * * ? ") //间隔1小时
	@TaskLock(key = RedisConstant.PARKING_RESULT_LOCK_KEY)
	public void syncLotteryResultState() {
		try {
			//如果都同步成功，则吧状态改成待发布
			List<LotteryResultPO> resultList = lotteryResultService.selectResultListByState(LotteryResultStateEnum.CONFIRM_IN_PROCESS.getState());
			log.info("确认中的结果数据{}",JSON.toJSONString(resultList));
			resultList.forEach(result -> {
				long num = userSpaceService.queryUnSyncListByBatch(result.getBatchId(),result.getRoundId());
				if (num == 0) {
					result.setState(LotteryResultStateEnum.UNPUBLIC.getState());
					result.setUpdateTm(new Date());
					lotteryResultService.updateById(result);
				}
			});
		} catch (Exception e) {
			log.error("同步结果状态失败：{}",e);
		}
	}
	
}
