package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author: lpy
 * @Date: 2022/10/31
 */
@Data
public class OrderCommitRsp {
    /**
     * 订单号
     */
    @ApiModelProperty(value = "orderId")
    private Long parkingOrderId;
}
