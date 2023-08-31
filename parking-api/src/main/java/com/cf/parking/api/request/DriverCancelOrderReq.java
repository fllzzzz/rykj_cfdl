package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Data
public class DriverCancelOrderReq {
    @ApiModelProperty(value = "订单号")
    private Long parkingOrderId;
}

