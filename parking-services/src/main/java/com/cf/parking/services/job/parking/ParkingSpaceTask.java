package com.cf.parking.services.job.parking;


import com.cf.parking.dao.mapper.UserSpaceMapper;
import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.facade.facade.ParkingFacade;
import com.cf.parking.facade.facade.UserSpaceFacade;
import com.cf.parking.services.integration.GatewayHikvisionFeign;
import com.cf.parking.services.job.annotation.TaskLock;
import com.cf.parking.services.service.HikvisionService;
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
 * 停车位数据同步
 */
@Slf4j
@Component
public class ParkingSpaceTask {

    @Value("${spring.profiles.active}")
    private String profiles;

    @Resource
    private HikvisionService hikvisionService;
    @Resource
    private GatewayHikvisionFeign gatewayHikvisionFeign;
    @Resource
    private RedissonUtil redissonUtil;
    @Resource
    private UserSpaceFacade userSpaceFacade;
    @Resource
    private UserSpaceMapper userSpaceMapper;

    @Resource
    private ParkingFacade parkingFacade;
    /**
     * 车位信息同步
     */
    @Async
//    @Scheduled(cron = "0 10 0 * * ? ") //零点十分
    @Scheduled(cron = "0 10 0,3 * * ?") //零点十分 + 3点十分
//    @Scheduled(cron = "0/5 * * * * ?")
    public void getUserSpace() {
        //防止两台机器同时跑任务
        String lockKey = RedisConstant.JOB_USER_SPACE_LOCK_KEY;
        RLock rLock = redissonUtil.getRLock(lockKey);
        try {
            if (!redissonUtil.tryLock(rLock, RedisConstant.JOB_USER_SPACE_LOCK_KEY_WAIT, RedisConstant.JOB_USER_SPACE_LOCK_KEY_EXPIRE, TimeUnit.MINUTES)) {
                log.info("userSpace-repeat");
                return;
            }
            String traceId = UUID.randomUUID().toString().replaceAll("-", "").toUpperCase();
            MDC.put("traceId", traceId);

            long tm = System.currentTimeMillis();
            log.info("UserSpaceTaskStart");

            // String endTime = DateUtil.format(new Date(), "yyyy-MM-dd HH:mm:ss").replace(" ", "T") + "+08:00";
            userSpaceFacade.syncUserSpaceData();

            log.info("UserSpaceTaskEnd,rt={}", (System.currentTimeMillis() - tm));
        } catch (Exception e) {
            log.error("UserSpaceTaskErr", e);
            DingAlarmUtils.alarmException("UserSpaceTaskErr" + e.getMessage());
        } finally {
            try {
                redissonUtil.unlock(rLock);
            } catch (Exception e) {
                log.error("解锁异常,key:[{}],e:", lockKey, e);
            }
            MDC.clear();
        }
    }
    /**
     * 僵尸车
     */
    @Scheduled(cron = "0 4 0/1 * * ? ") //间隔1小时
    @TaskLock(key = RedisConstant.PARKING_JOB_ZOMBIE_LOCK_KEY)
    //@Scheduled(fixedDelay = 6000000) //间隔1小时
    @Async
    public void zombieVehicle() {
        parkingFacade.dealZombieVehicle();
        //parkingFacade.dealNoParking();
    }
    /**
     * 不停车
     */
    @Scheduled(cron = "0 4 0/1 * * ? ") //间隔1小时
    @TaskLock(key = RedisConstant.PARKING_JOB_NOT_PARK_LOCK_KEY)
    //@Scheduled(fixedDelay = 6000000) //间隔1小时
    @Async
    public void notParked() {
        parkingFacade.dealNoParking();
    }



    //    /**
//     * 更新车位状态
//     * 旧parkingfree 在新分支注释掉
//     */
//    @Async
//    @Scheduled(cron = "0 0/10 * * * ? ") //间隔十分钟
//    public void sycnData() {
//        //防止两台机器同时跑任务
//        String lockKey = RedisConstant.JOB_SYCNDATA_LOCK_KEY;
//        RLock rLock = redissonUtil.getRLock(lockKey);
//        try {
//            if (!redissonUtil.tryLock(rLock, RedisConstant.JOB_SYCNDATA_LOCK_KEY_WAIT, RedisConstant.JOB_SYCNDATA_LOCK_KEY_EXPIRE, TimeUnit.MINUTES)) {
//                log.info("sycnData-repeat");
//                return;
//            }
//            String traceId = UUID.randomUUID().toString().replaceAll("-", "").toUpperCase();
//            MDC.put("traceId", traceId);
//            if(!"test".equals(profiles)){
//                return;
//            }
//            long tm = System.currentTimeMillis();
//            log.info("ParkingSpaceTaskStart");
//            hikvisionService.sycnData();
//            log.info("ParkingSpaceTaskEnd,rt={}", (System.currentTimeMillis() - tm));
//        } catch (Exception e) {
//            log.error("ParkingSpaceTaskErr", e);
//            DingAlarmUtils.alarmException("ParkingSpaceTaskErr" + e.getMessage());
//        }finally {
//            try {
//                redissonUtil.unlock(rLock);
//            } catch (Exception e) {
//                log.error("解锁异常,key:[{}],e:", lockKey, e);
//            }
//            MDC.clear();
//        }
//    }

}