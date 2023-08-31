package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @author: lpy
 * @Date: 2022/11/10
 */
@Data
@Accessors(chain = true)
public class ConfirmRidingDTO {
    /**
     * 订单号
     */
    private Long parkingOrderId;
    /**
     * 乘客起始地经度
     */
    private BigDecimal passengerStartLongitude;
    /**
     * 乘客起始地纬度
     */
    private BigDecimal passengerStartLatitude;

    private Long userId;

}
