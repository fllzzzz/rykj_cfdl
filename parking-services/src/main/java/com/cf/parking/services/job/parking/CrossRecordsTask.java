package com.cf.parking.services.job.parking;

import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.facade.facade.CrossRecordsFacade;
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
 * @date 2023/3/29
 */
@Slf4j
@Component
public class CrossRecordsTask {

    @Value("${spring.profiles.active}")
    private String profiles;
    @Resource
    private RedissonUtil redissonUtil;
    @Resource
    private CrossRecordsFacade crossRecordsFacade;

    @Async
    @Scheduled(cron = "0 0/60 * * * ? ")
    public void getCrossRecords() {
        //防止两台机器同时跑任务
        String lockKey = RedisConstant.PARKING_JOB_CROSS_RECORDS_LOCK_KEY;
        RLock rLock = redissonUtil.getRLock(lockKey);
        try {
            if (!redissonUtil.tryLock(rLock, RedisConstant.USER_LOGIN_LOCK_KEY_WAIT, RedisConstant.USER_LOGIN_LOCK_KEY_EXPIRE, TimeUnit.MINUTES)) {
                log.info("crossRecordsTaskEvent---repeat");
                return;
            }
            String traceId = UUID.randomUUID().toString().replaceAll("-", "").toUpperCase();
            MDC.put("traceId", traceId);

            //TODO 测试
            //if (!"prod".equals(profiles)) {
            //    return;
            //}
            long tm = System.currentTimeMillis();

            log.info("crossRecordsTaskEventStart");
            Integer pageNo = 1;
            Integer total = crossRecordsFacade.saveCrossRecords(pageNo);
            for (int i = 2; i <= total / 1000 + 1; i++) {
                crossRecordsFacade.saveCrossRecords(i);
                Thread.sleep(100);
            }
            log.info("crossRecordsTaskEventEnd,rt={}", (System.currentTimeMillis() - tm));
        } catch (Exception e) {
            log.error("crossRecordsTaskEventErr", e);
            DingAlarmUtils.alarmException("crossRecordsTaskEventErr" + e.getMessage());
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
