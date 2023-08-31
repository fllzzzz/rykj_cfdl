package com.cf.parking.services.job.order;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;

/**
 * 订单专用延迟工具类
 * @Classname OrderDelyJobUtil
 * @Date 2022/10/21 15:53
 * @Created by csy
 */
public class OrderDelayJobUtil {
    /**
     * 需要提醒的Id列表，用于减少重复扫描的风险
     */
    public static Set<Long> needNoticeOrderId = new CopyOnWriteArraySet<>();
    /**
     * 延迟线程池
     */
    public static ScheduledExecutorService scheduledExecutorService = new ScheduledThreadPoolExecutor(20);

}
