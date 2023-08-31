package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @author whx
 * @date 2022/11/11
 */
@Data
@Accessors(chain = true)
public class CommonAddressDTO {

	/**
	 * 省
	 */
	private String province;

	/**
	 * 市
	 */
	private String city;

	/**
	 * 县
	 */
	private String adName;

	/**
	 * 详细地址
	 */
	private String title;

	/**
	 * 经度
	 */
	private BigDecimal longitude;

	/**
	 * 纬度
	 */
	private BigDecimal latitude;
}
