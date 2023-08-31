package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import lombok.Data;

import java.util.Date;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Data
public class AdminOrderRecordDTO extends PageRequest {
    /**
     * 出发时间
     */
    private Date orderDate;

    /**
     * 订单状态(1:未开始,2:进行中,3:已取消,4.已完成)
     */
    private Integer orderState;

    private Long parkingOrderId;

    /**
     * 司机姓名
     */
    private String driverName;
}
