package com.cf.parking.facade.dto;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author: lpy
 * @Date: 2022/11/10
 */
@Data
public class PassengerArriveDTO {
    private Long parkingOrderId;

    /**
     * 乘客到达地经度
     */
    private BigDecimal passengerDestLongitude;

    /**
     * 乘客到达地纬度
     */
    private BigDecimal passengerDestLatitude;
}
