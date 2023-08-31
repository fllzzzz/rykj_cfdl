package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author whx
 * @date 2022/10/19
 */
@Data
@Accessors(chain = true)
public class PendingBO {
    private Long parkingOrderId;

    private Date orderTime;

    /**
     * 起始地
     */
    private String startAddress;

    /**
     * 目的地
     */
    private String destAddress;

    /**
     * 时间格式化
     */
    private String orderTimeShow;

    /**
     * 订单状态返回消息
     */
    private String orderStateMsg;
}
