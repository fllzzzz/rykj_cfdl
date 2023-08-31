package com.cf.parking.api.request;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2022/10/25
 */
@Data
@Accessors(chain = true)
public class OrderIdReq {
	private Long parkingOrderId;
}
