package com.cf.parking.facade.bo;

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
public class AdminOrderDetailBO {
    private Long parkingOrderId;

    private String startAddress;

    private String destAddress;

    private String passengerInfoList;

    private Date orderTime;

    private String driverJobNumber;

    private String driverName;

    private Integer orderState;

    private BigDecimal distance;

    private BigDecimal startDestDistance;

    /**
     * 乘客上车地距离起始地距离
     */
    private BigDecimal startDistance;

    /**
     * 乘客下车地距离目的地距离
     */
    private BigDecimal destDistance;

    /**
     * 乘客上车时间
     */
    private Date getInTm;

    /**
     * 乘客下车时间
     */
    private Date getOffTm;

    /**
     * 乘客取消：1，司机取消：2
     */
    private Integer cancelUserType;

    /**
     * 操作时间
     */
    private Date cancelTime;

    /**
     * 评价司机
     */
    private String driverEvaluateDesc;

    /**
     * 评价司机level
     */
    private Integer driverLevel;

    /**
     * 评价乘客
     */
    private String passengerEvaluateDesc;

    /**
     * 评价乘客level
     */
    private Integer passengerLevel;

    /**
     * 备注
     */
    private String remark;

}
