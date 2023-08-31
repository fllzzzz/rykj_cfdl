package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * @author: lpy
 * @Date: 2022/10/19
 */
@Data
public class DriverOrderUnfinishedRsp {
    @ApiModelProperty(value = "订单号")
    private Long parkingOrderId;

    @ApiModelProperty(value = "出发时间")
    private Date orderTime;

    @ApiModelProperty(value = "起始地")
    private String startAddress;

    @ApiModelProperty(value = "目的地")
    private String destAddress;

    @ApiModelProperty(value = "时间格式化")
    private String orderTimeShow;

    @ApiModelProperty(value = "订单状态返回消息")
    private String orderStateMsg;
}
