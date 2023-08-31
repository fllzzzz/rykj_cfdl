package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Data
@Accessors(chain = true)
public class DriverEvaluateReq {
    /**
     * 星级(1:*,2:**,:3:***,4:****,5:*****)
     */
    @ApiModelProperty(value = "星级(1:*,2:**,:3:***,4:****,5:*****)")
    private Integer level;

    /**
     * 评价
     */
    @ApiModelProperty(value = "评价")
    private String evaluateDesc;

    @ApiModelProperty(value = "订单号")
    private Long parkingOrderId;

    /**
     * 被评用户id
     */
    @ApiModelProperty(value = "评价")
    private Long userId;
}
