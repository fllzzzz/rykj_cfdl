package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author whx
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class CarChargeDelDTO implements Serializable {
	/**
	 * 停车库唯一标识
	 */
	private String parkSyscode;

	/**
	 * 车牌号码
	 */
	private String plateNo;
}