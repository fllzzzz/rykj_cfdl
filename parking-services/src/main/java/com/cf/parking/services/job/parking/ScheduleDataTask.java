package com.cf.parking.services.job.parking;

import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.facade.facade.ScheduleDataFacade;
import com.cf.parking.services.job.annotation.TaskLock;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * @author whx
 * @date 2023/3/28
 */
@Slf4j
@Component
public class ScheduleDataTask {
    @Resource
    private ScheduleDataFacade scheduleDataFacade;

    @Async
    @Scheduled(cron = "0 0 2 * * ?") // 每天2点触发
    @TaskLock(key = RedisConstant.PARKING_JOB_SCHEDULE_LOCK_KEY)
    public void getScheduleData() {
        scheduleDataFacade.getGaiaAttendance();
    }
}
