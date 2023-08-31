package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;

/**
 * @author: lpy
 * @Date: 2022/11/10
 */
@Data
public class PassengerArriveReq {
    @ApiModelProperty(value = "订单id")
    private Long parkingOrderId;

    @ApiModelProperty(value = "乘客到达地经度")
    private BigDecimal passengerDestLongitude;

    @ApiModelProperty(value = "乘客到达地纬度")
    private BigDecimal passengerDestLatitude;
}
