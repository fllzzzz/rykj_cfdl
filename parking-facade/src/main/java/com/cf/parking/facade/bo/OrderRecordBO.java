package com.cf.parking.facade.bo;

import lombok.Data;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Data
public class OrderRecordBO {
    private Long parkingOrderId;

    private Long userId;

    private String jobNumber;

    private String name;

    private String startProvince;

    private String startCity;

    private String startCounty;

    private String startAddress;

    private String destProvince;

    private String destCity;

    private String destCounty;

    private String destAddress;

    private Date orderTime;

    private Integer passengerNum;

    private String remark;

    private Integer orderState;

    /**
     * 评价状态
     */
    private Integer evaluateState;

    private Long passengerUserId;

    /**
     * 时间格式化
     */
    private String orderTimeShow;

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
     * 订单起始点目的地距离
     */
    private BigDecimal distance;
}
