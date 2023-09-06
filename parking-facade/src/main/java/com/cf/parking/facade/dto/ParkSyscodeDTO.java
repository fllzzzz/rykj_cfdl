package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class ParkSyscodeDTO {
	/**
	 * 停车库唯一标识
	 */
	private String parkSyscode;
}
