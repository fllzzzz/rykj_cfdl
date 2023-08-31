package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Data
public class AdminOrderRecordRsp {
    @ApiModelProperty(value = "订单号")
    private Long parkingOrderId;

    @ApiModelProperty(value = "起始地")
    private String startAddress;

    @ApiModelProperty(value = "目的地")
    private String destAddress;

    @ApiModelProperty(value = "公里数")
    private BigDecimal distance;

    @ApiModelProperty(value = "出发时间")
    private Date orderTime;

    @ApiModelProperty(value = "司机工号")
    private String driverJobNumber;

    @ApiModelProperty(value = "司机姓名")
    private String driverName;

    @ApiModelProperty(value = "乘客列表")
    private String passengerInfoList;

    @ApiModelProperty(value = "订单状态(1:未开始,2:进行中,3:已取消,4.已完成)")
    private Integer orderState;
}
