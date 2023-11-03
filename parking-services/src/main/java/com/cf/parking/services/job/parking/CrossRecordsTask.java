package com.cf.parking.services.job.parking;

import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.facade.facade.CrossRecordsFacade;
import com.cf.support.utils.DingAlarmUtils;
import com.cf.support.utils.RedisUtil;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.UUID;

/**
 * @author whx
 * @date 2023/3/29
 */
@Slf4j
@Component
public class CrossRecordsTask {

    @Resource
    private RedisUtil redisUtil;
    @Resource
    private CrossRecordsFacade crossRecordsFacade;

    @Async
    @Scheduled(cron = "0 0/60 * * * ? ")
    public void getCrossRecords() {
        //防止两台机器同时跑任务
        String lockKey = RedisConstant.PARKING_JOB_CROSS_RECORDS_LOCK_KEY;
        if (!redisUtil.lock(lockKey, "1", 300)) {
            log.info("crossRecordsTaskEvent---repeat");
            return;
        }
        try {

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
            MDC.clear();
        }
    }
}
