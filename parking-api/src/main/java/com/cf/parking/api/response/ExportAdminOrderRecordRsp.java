package com.cf.parking.api.response;

import cn.afterturn.easypoi.excel.annotation.Excel;
import lombok.Data;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Data
public class ExportAdminOrderRecordRsp {

    @Excel(name = "订单id", orderNum = "1")
    private Long parkingOrderId;

    @Excel(name = "出发地址", orderNum = "2")
    private String startAddress;

    @Excel(name = "目的地址", orderNum = "3")
    private String destAddress;

    @Excel(name = "行驶距离(KM)", orderNum = "4")
    private BigDecimal distance;

    @Excel(name = "乘客乘坐距离(KM)", orderNum = "5")
    private BigDecimal startDestDistance;

    @Excel(name = "乘客上车地至起始地距离(KM)", orderNum = "6")
    private BigDecimal startDistance;

    @Excel(name = "乘客下车地至目的地距离(KM)", orderNum = "6")
    private BigDecimal destDistance;

    @Excel(name = "乘客上车时间", orderNum = "7")
    private String getInTm;

    @Excel(name = "乘客下车时间", orderNum = "7")
    private String getOffTm;

    @Excel(name = "出发时间", orderNum = "8")
    private String orderTime;

    @Excel(name = "司机工号", orderNum = "9")
    private String driverJobNumber;

    @Excel(name = "司机姓名", orderNum = "9")
    private String driverName;

    /**
     * 订单状态(1:未开始,2:进行中,3:已取消,4.已完成,5.已超时)
     */
    @Excel(name = "订单状态", orderNum = "10")
    private String orderState;

    @Excel(name = "用户信息", orderNum = "11")
    private String passengerInfoList;
}
