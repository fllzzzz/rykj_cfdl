package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Data
@Accessors(chain = true)
public class DriverEvaluateDTO {
    /**
     * 星级(1:*,2:**,:3:***,4:****,5:*****)
     */
    private Integer level;

    /**
     * 评价
     */
    private String evaluateDesc;

    private Long parkingOrderId;

    /**
     * 被评用户id
     */
    private Long userId;
    /**
     * 评价者id
     */
    private Long driverUserId;
}
