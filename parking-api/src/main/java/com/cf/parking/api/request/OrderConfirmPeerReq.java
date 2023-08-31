package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author: lpy
 * @Date: 2022/10/19
 */
@Data
@Accessors(chain = true)
public class OrderConfirmPeerReq {
    @ApiModelProperty(value = "同行记录id")
    private Long orderPeerId;

    @ApiModelProperty(value = "订单号")
    private Long parkingOrderId;
}
