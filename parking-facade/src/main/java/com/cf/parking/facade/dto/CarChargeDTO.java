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
public class CarChargeDTO implements Serializable {
	/**
	 * 停车库唯一标识
	 */
	private String parkSyscode;

	/**
	 * 车牌号码
	 */
	private String plateNo;

	/**
	 * 开始时间
	 * 时间格式：yyyy-MM-dd，如：2018-07-26
	 */
	private String startTime;

	/**
	 * 结束时间
	 * 时间格式：yyyy-MM-dd，如：2018-07-26
	 */
	private String endTime;
}
