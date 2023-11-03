package com.cf.parking.services.job.parking;

import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.facade.facade.CrossRecordsFacade;
import com.cf.parking.services.job.annotation.TaskLock;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * @author whx
 * @date 2023/3/29
 */
@Slf4j
@Component
public class CrossRecordsTask {
    @Resource
    private CrossRecordsFacade crossRecordsFacade;

    @Async
    @Scheduled(cron = "0 0 0/1 * * ? ")//1小时一次
    @TaskLock(key = RedisConstant.PARKING_JOB_CROSS_RECORDS_LOCK_KEY)
    public void getCrossRecords() throws InterruptedException {
        Integer pageNo = 1;
        Integer total = crossRecordsFacade.saveCrossRecords(pageNo);
        for (int i = 2; i <= total / 1000 + 1; i++) {
            crossRecordsFacade.saveCrossRecords(i);
            Thread.sleep(100);
        }
    }
}
