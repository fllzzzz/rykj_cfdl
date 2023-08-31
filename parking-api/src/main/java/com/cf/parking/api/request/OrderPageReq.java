package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @author whx
 * @date 2022/10/19
 */
@Data
@Accessors(chain = true)
public class OrderPageReq extends PageRequest {
	/**
	 * 乘客起始地经度
	 */
	private BigDecimal passengerStartLongitude;

	/**
	 * 乘客起始地纬度
	 */
	private BigDecimal passengerStartLatitude;
	/**
	 * 乘客目的地经度
	 */
	private BigDecimal passengerDestLongitude;

	/**
	 * 乘客目的地纬度
	 */
	private BigDecimal passengerDestLatitude;
}
