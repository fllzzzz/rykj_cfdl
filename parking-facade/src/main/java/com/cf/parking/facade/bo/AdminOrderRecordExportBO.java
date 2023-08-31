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
public class AdminOrderRecordExportBO {
    private Long parkingOrderId;

    private String startAddress;

    private String destAddress;

    private BigDecimal distance;

    /**
     * 乘客出发地距离乘客到达地距离
     */
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
    private String getInTm;

    /**
     * 乘客下车时间
     */
    private String getOffTm;

    private String passengerInfoList;

    private String orderTime;

    private String driverJobNumber;

    private String driverName;

    private String orderState;

}
