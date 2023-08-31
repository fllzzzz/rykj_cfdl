package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author: lpy
 * @Date: 2022/10/19
 */
@Data
@Accessors(chain = true)
public class OrderConfirmPeerDTO {
    private Long orderPeerId;

    private Long parkingOrderId;

    private Long userId;
}
