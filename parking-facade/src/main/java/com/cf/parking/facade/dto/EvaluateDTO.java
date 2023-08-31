package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2022/10/21
 */
@Data
@Accessors(chain = true)
public class EvaluateDTO {
	private Long parkingOrderId;

	private Long userId;

	private Integer level;

	private String EvaluateDesc;
}
