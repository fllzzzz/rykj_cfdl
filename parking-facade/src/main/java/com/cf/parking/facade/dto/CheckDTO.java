package com.cf.parking.facade.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @author: lpy
 * @Date: 2022/11/11
 */
@Data
@Accessors(chain = true)
public class CheckDTO {
    @ApiModelProperty(value = "起始点或目的地经度")
    private BigDecimal longitude;

    @ApiModelProperty(value = "起始点或目的地纬度")
    private BigDecimal latitude;

    @ApiModelProperty(value = "订单号")
    private Long parkingOrderId;
}
