package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;
/**
 * @author lpy
 * @date 2022/10/19
 */

@Data
@Accessors(chain = true)
public class ParkingOrderDTO {
    /**
     * 订单号
     */
    private Long parkingOrderId;

    /**
     * userId
     */
    private Long userId;

    /**
     * 起始地省
     */
    private String startProvince;

    /**
     * 起始地市
     */
    private String startCity;

    /**
     * 起始地区
     */
    private String startCounty;

    /**
     * 起始地详细地址
     */
    private String startAddress;

    /**
     * 目的地省
     */
    private String destProvince;

    /**
     * 目的地市
     */
    private String destCity;

    /**
     * 目的地区
     */
    private String destCounty;

    /**
     * 目的地详细地址
     */
    private String destAddress;

    /**
     * 出发时间
     */
    private Date orderTime;

    /**
     * 乘客人数
     */
    private Integer passengerNum;

    /**
     * 备注
     */
    private String remark;

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
     * 订单状态
     */
    private Integer orderState;
}
