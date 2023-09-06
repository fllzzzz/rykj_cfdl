package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class ParkListDTO {
	/**
	 * 停车场唯一标识
	 */
	private String parkIndexCode;

	/**
	 * 停车场名称
	 */
	private String parkName;

	/**
	 * 父停车场唯一标识
	 */
	private String parentParkIndexCode;

	/**
	 * 创建时间
	 */
	private String createTime;

	/**
	 * 更新时间
	 */
	private String updateTime;

}
