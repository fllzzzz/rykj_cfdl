package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import lombok.Data;

import java.math.BigDecimal;

/**
 * @author: lpy
 * @Date: 2022/11/10
 */
@Data
public class RidingOrderDTO extends PageRequest {
    /**
     * 乘客起始地经度
     */
    private BigDecimal passengerStartLongitude;

    /**
     * 乘客起始地纬度
     */
    private BigDecimal passengerStartLatitude;
    /**
     * 乘客目的地经度
     */
    private BigDecimal passengerDestLongitude;

    /**
     * 乘客目的地纬度
     */
    private BigDecimal passengerDestLatitude;
}
