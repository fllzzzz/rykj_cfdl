package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @Classname OrderTimeTaskEntity
 * @Date 2022/10/19 15:38
 * @Created by csy
 */
@Data
@Accessors(chain = true)
public class OrderTimeTaskDTO {
    /**
     * 订单id
     */
    private Long parkingOrderId;
    /**
     * 多少分钟后运行
     */
    private Long runMinute;



}
