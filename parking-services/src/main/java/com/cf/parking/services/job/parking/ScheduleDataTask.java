package com.cf.parking.services.job.parking;

import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.facade.facade.ScheduleDataFacade;
import com.cf.support.utils.DingAlarmUtils;
import com.cf.support.utils.RedissonUtil;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

/**
 * @author whx
 * @date 2023/3/28
 */
@Slf4j
@Component
public class ScheduleDataTask {

	@Value("${spring.profiles.active}")
	private String profiles;
	@Resource
	private RedissonUtil redissonUtil;
	@Resource
	private ScheduleDataFacade scheduleDataFacade;

	@Async
	@Scheduled(cron = "0 0 2 * * ?") // 每天2点触发
	public void getScheduleData() {

		String lockKey = RedisConstant.PARKING_JOB_SCHEDULE_LOCK_KEY;
		RLock rLock = redissonUtil.getRLock(lockKey);
		try {
			if (!redissonUtil.tryLock(rLock, RedisConstant.USER_LOGIN_LOCK_KEY_WAIT, RedisConstant.USER_LOGIN_LOCK_KEY_EXPIRE, TimeUnit.MINUTES)) {
				log.info("getScheduleDataTask---repeat");
				return;
			}
			String traceId = UUID.randomUUID().toString().replaceAll("-", "").toUpperCase();
			MDC.put("traceId", traceId);

			//TODO 测试
			//if (!"prod".equals(profiles)) {
			//	return;
			//}
			long tm = System.currentTimeMillis();

			log.info("getScheduleDataTask start");
			scheduleDataFacade.getGaiaAttendance();
			log.info("getScheduleDataTask end,rt={}", (System.currentTimeMillis() - tm));
		} catch (Exception e) {
			log.error("getScheduleDataTask", e);
			DingAlarmUtils.alarmException("getScheduleDataTaskErr" + e.getMessage());
		} finally {
			try {
				redissonUtil.unlock(rLock);
			} catch (Exception e) {
				log.error("解锁异常,key:[{}],e:", lockKey, e);
			}
			MDC.clear();
		}
	}
}
