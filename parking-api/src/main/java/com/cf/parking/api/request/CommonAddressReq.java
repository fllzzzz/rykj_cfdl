package com.cf.parking.api.request;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author whx
 * @date 2022/11/11
 */
@Data
public class CommonAddressReq {
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
