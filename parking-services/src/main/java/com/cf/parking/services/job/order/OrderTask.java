package com.cf.parking.services.job.order;


import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.facade.facade.OrderDelayFacade;
import com.cf.parking.facade.facade.OrderPeerFacade;
import com.cf.parking.facade.facade.ParkingOrderFacade;
import com.cf.parking.services.job.annotation.TaskLock;
import com.cf.support.utils.DingAlarmUtils;
import com.cf.support.utils.RedisUtil;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.UUID;

/**
 * @Classname OrderTask
 * @Date 2022/10/19 8:55
 * @Created by csy
 */
@Slf4j
@Component
public class OrderTask {

    @Value("${server.port}")
    private int serverPort;
    @Resource
    private OrderPeerFacade orderPeerFacade;
    @Resource
    private ParkingOrderFacade parkingOrderFacade;
    @Resource
    private RedisUtil redisUtil;

    @Resource
    public OrderDelayFacade orderDelayFacade;


    /*    *//**
     * 获取
     *//*
    @Scheduled(cron = "0 0 0/1 * * ? ")
    //@Scheduled(fixedDelay = 600000)
    public void automaticOrderTask() {
        String lockKey = RedisConstant.ORDER_JOB_AUTOMATIC_ORDER_LOCK_KEY;
        RLock rLock = redissonUtil.getRLock(lockKey);
        try {
            if (!redissonUtil.tryLock(rLock, RedisConstant.JOB_SYCNDATA_LOCK_KEY_WAIT, RedisConstant.JOB_SYCNDATA_LOCK_KEY_EXPIRE, TimeUnit.MINUTES)) {
                log.info("automaticOrderTask-repeat");
                return;
            }
            log.info("------------automaticOrderTask start--------------");
            String traceId = UUID.randomUUID().toString().replaceAll("-", "").toUpperCase();
            MDC.put("traceId", traceId);
            orderDelayFacade.automaticOrder();
            log.info("------------automaticOrderTask end--------------");
        } catch (Exception e) {
            log.error("automaticOrderTaskErr", e);
            DingAlarmUtils.alarmException("automaticOrderTaskErr" + e.getMessage());
        } finally {
            try {
                redissonUtil.unlock(rLock);
            } catch (Exception e) {
                log.error("解锁异常,key:[{}],e:", lockKey, e);
            }
            MDC.clear();
        }
    }*/


    /**
     * 月末添加积分（表达式换成月初处理，月末计算最后一天有误）
     */ /*
    @Scheduled(cron = "0 0 0 1 * ? ")
    @TaskLock(key = RedisConstant.ORDER_JOB_COUNT_LOCK_KEY)
    public void autoCountIntegralTask() {
        orderDelayFacade.autoCountIntegral();
    } */

    /**
     * 自动过期订单
     */
    @Scheduled(cron = "0 0 0/1 * * ? ")
    @TaskLock(key = RedisConstant.ORDER_JOB_ARRIVE_ORDER_LOCK_KEY)
    //@Scheduled(fixedDelay = 600000)
    public void autoOutOrderTask() {
        orderDelayFacade.autoOutOrder();
    }

    /**
     * 提醒还未确认的订单
     */
    @Scheduled(cron = "0 0 10,20 * * ? ")
    @TaskLock(key = RedisConstant.ORDER_JOB_SYCN_NO_SURE_KEY)
    public void notSureOrderTask() {

        orderPeerFacade.noticeNoSureOrder();


    }

    /**
     * 取消15分钟还没确定人同行的订单以及提醒
     */
    @Scheduled(cron = "0 0/5 * * * ? ")
    //@Scheduled(fixedDelay = 200000)
    public void dealOutTimeOrderTask() {
        try {
            if (tryLock()) {
                String traceId = UUID.randomUUID().toString().replaceAll("-", "").toUpperCase();
                MDC.put("traceId", traceId);
                log.info("------------dealOutTimeOrderTask start--------------");
                parkingOrderFacade.dealOutTimeAndStartOrder();
                log.info("------------dealOutTimeOrderTask end--------------");
            }
        } catch (Exception e) {
            log.error("dealOutTimeOrderTaskErr", e);
            DingAlarmUtils.alarmException("dealOutTimeOrderTaskErr" + e.getMessage());
        } finally {

            MDC.clear();
        }

    }


    /**
     * 加锁，不释放，只允许一个实例执行
     *
     * @return 加锁是否成功
     */
    private boolean tryLock() {
        InetAddress address = null;
        try {
            address = InetAddress.getLocalHost();
        } catch (UnknownHostException e) {
            log.error("get ip error", e);
        }
        assert address != null;
        String ipAddress = address.getHostAddress() + ":" + serverPort;
        log.info("tryLock-ipAddress: {}", ipAddress);
        if (redisUtil.lock(RedisConstant.ORDER_JOB_SYCN_DATA_LOCK_KEY, ipAddress, 60 * 6)) {
            //抢占
            return true;
        } else {
            //抢占的锁的ip
            String occupationIp = redisUtil.get(RedisConstant.ORDER_JOB_SYCN_DATA_LOCK_KEY);
            //只允许抢占锁的应用刷新
            if (occupationIp.equals(ipAddress)) {
                redisUtil.expire(RedisConstant.ORDER_JOB_SYCN_DATA_LOCK_KEY, 60 * 6);
                return true;
            }
            return false;
        }


    }


}
