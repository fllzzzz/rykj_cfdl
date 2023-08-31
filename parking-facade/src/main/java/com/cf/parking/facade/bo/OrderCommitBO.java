package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author: lpy
 * @Date: 2022/10/31
 */
@Data
@Accessors(chain = true)
public class OrderCommitBO {
    /**
     * 订单号
     */
    private Long parkingOrderId;
}
