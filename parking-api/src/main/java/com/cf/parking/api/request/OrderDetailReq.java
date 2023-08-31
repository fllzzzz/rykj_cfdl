package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
/**
 * @author lpy
 * @date 2022/10/19
 */

@Data
public class OrderDetailReq {
    @ApiModelProperty(value = "订单号")
    private Long parkingOrderId;
}
