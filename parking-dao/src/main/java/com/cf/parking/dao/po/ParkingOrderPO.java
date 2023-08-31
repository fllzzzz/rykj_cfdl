package com.cf.parking.dao.po;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @author whx
 * @date 2022-11-10 10:18:13
 * @description 顺风车订单表
 */
@Data
@TableName("parking_order")
@Accessors(chain = true)
public class ParkingOrderPO {
    /**
     * 订单号
     */
    @TableId(value = "parking_order_id", type = IdType.INPUT)
    private Long parkingOrderId;

    /**
     * 司机ID
     */
    private Long userId;

    /**
     * 司机工号
     */
    private String jobNumber;

    /**
     * 司机姓名
     */
    private String name;

    /**
     * 起始地 省
     */
    private String startProvince;

    /**
     * 起始地 市
     */
    private String startCity;

    /**
     * 起始地 县
     */
    private String startCounty;

    /**
     * 起始地 详细地址
     */
    private String startAddress;

    /**
     * 目的地 省
     */
    private String destProvince;

    /**
     * 目的地 市
     */
    private String destCity;

    /**
     * 目的地 县
     */
    private String destCounty;

    /**
     * 目的地 详细地址
     */
    private String destAddress;

    /**
     * 出发时间
     */
    private Date orderTime;

    /**
     * 可乘车人数
     */
    private Integer passengerNum;

    /**
     * 备注
     */
    private String remark;

    /**
     * 是否已通知 1：未通知  2：已通知  
     */
    private Integer noticed;

    /**
     * 订单状态(1:未开始,2:进行中,3:已取消,4.已完成)
     */
    private Integer orderState;

    /**
     * 起始地经度
     */
    private BigDecimal startLongitude;

    /**
     * 起始地纬度
     */
    private BigDecimal startLatitude;

    /**
     * 目的地经度
     */
    private BigDecimal destLongitude;

    /**
     * 目的地纬度
     */
    private BigDecimal destLatitude;

    /**
     * 行驶距离
     */
    private BigDecimal distance;

    /**
     * 乘客选择起始点距离司机起始点距离
     */
    @TableField(exist = false)
    private BigDecimal startDistance;

    /**
     * 乘客选择下车点距离司机下车距离
     */
    @TableField(exist = false)
    private BigDecimal destDistance;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;
}
