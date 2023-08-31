package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @author: lpy
 * @Date: 2022/10/21
 */
@Data
@Accessors(chain = true)
public class AdminOrderDetailRsp {
    @ApiModelProperty(value = "订单号")
    private Long parkingOrderId;

    @ApiModelProperty(value = "起始地")
    private String startAddress;

    @ApiModelProperty(value = "目的地")
    private String destAddress;

    @ApiModelProperty(value = "乘客列表")
    private String passengerInfoList;

    @ApiModelProperty(value = "预订出发时间")
    private Date orderTime;

    @ApiModelProperty(value = "司机工号")
    private String driverJobNumber;

    @ApiModelProperty(value = "司机姓名")
    private String driverName;

    @ApiModelProperty(value = "订单状态")
    private Integer orderState;

    @ApiModelProperty(value = "订单起始距离")
    private BigDecimal distance;

    @ApiModelProperty(value = "乘客乘坐距离(KM)")
    private BigDecimal startDestDistance;

    @ApiModelProperty(value = "乘客上车地距离起始地距离")
    private BigDecimal startDistance;

    @ApiModelProperty(value = "乘客下车地距离目的地距离")
    private BigDecimal destDistance;

    @ApiModelProperty(value = "乘客上车时间")
    private Date getInTm;

    @ApiModelProperty(value = "乘客下车时间")
    private Date getOffTm;

    /**
     * 乘客取消：1，司机取消：2
     */
    @ApiModelProperty(value = "乘客取消：1，司机取消：2")
    private Integer cancelUserType;

    /**
     * 操作时间
     */
    @ApiModelProperty(value = "操作时间")
    private Date cancelTime;

    /**
     * 评价司机
     */
    @ApiModelProperty(value = "评价司机")
    private String driverEvaluateDesc;

    /**
     * 评价乘客
     */
    @ApiModelProperty(value = "评价乘客")
    private String passengerEvaluateDesc;

    @ApiModelProperty(value = "备注")
    private String remark;

    @ApiModelProperty(value = "评价司机level")
    private Integer driverLevel;

    @ApiModelProperty(value = "评价乘客level")
    private Integer passengerLevel;

}
