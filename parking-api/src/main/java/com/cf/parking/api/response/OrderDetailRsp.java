package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * @author lpy
 * @date 2022/10/19
 */
@Data
public class OrderDetailRsp {
    @ApiModelProperty(value = "订单详情")
    private OrderRsp order;

    @ApiModelProperty(value = "用户详情")
    private UserRsp profile;

    @ApiModelProperty(value = "乘客列表")
    private List<OrderDetailPassengerPeerRsp> list;

    @ApiModelProperty(value = "取消状态(1:乘客2:司机)")
    private Integer cancelType;

    @ApiModelProperty(value = "操作状态(1:确认上车2:到达目的地)")
    private Integer optType;

    @ApiModelProperty(value = "用户类型(1:乘客2:司机)")
    private Integer userType;
}
