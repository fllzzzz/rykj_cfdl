package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Data
public class AdminOrderRecordReq extends PageRequest {
    /**
     * 出发时间
     */
    @ApiModelProperty(value = "出发时间")
    private Date orderDate;

    /**
     * 订单状态(1:未开始,2:进行中,3:已取消,4.已完成)
     */
    @ApiModelProperty(value = "订单状态(1:未开始,2:进行中,3:已取消,4.已完成)")
    private Integer orderState;

    @ApiModelProperty(value = "订单号")
    private Long parkingOrderId;

    /**
     * 司机姓名
     */
    @ApiModelProperty(value = "司机姓名")
    private String driverName;
}
