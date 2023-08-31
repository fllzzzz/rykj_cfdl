package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;


@Data
public class GeolocationReq {
    @ApiModelProperty(value = "订单id")
    private Long parkingOrderId;

    @ApiModelProperty(value = "经度")
    private BigDecimal longitude;

    @ApiModelProperty(value = "纬度")
    private BigDecimal latitude;
}
