package com.cf.parking.facade.facade;

import com.cf.parking.facade.dto.ParkingOrderDTO;

import java.util.List;

/**
 * 用于延时订单的处理
 * @Classname OrderExtendFacade
 * @Date 2022/10/21 16:28
 * @Created by csy
 */
public interface OrderDelayFacade {

    /**
     * 补偿机制，如果系统出现问题，未通知，小于这个时间的立即通知
     *
     * @param parkingOrderDTOList 订单列表
     */
    void compensate(List<ParkingOrderDTO> parkingOrderDTOList);

    /**
     * 延时处理超时订单和提醒要开始的订单
     * @param orderIdList 订单id李彪
     * @param runMinute  距离当前时间还有多久
     */
    void delayDealOutTimeAndStartOrder(List<Long> orderIdList,Long runMinute);

    /**
     * 自动过期订单
     */
    void autoOutOrder();

    /**
     * 自动评价
     */
    void automaticOrder();

    /**
     * 自动计算
     */
    void autoCountIntegral();
}
