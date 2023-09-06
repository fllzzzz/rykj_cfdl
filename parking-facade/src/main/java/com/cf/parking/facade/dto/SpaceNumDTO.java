package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class SpaceNumDTO {
	/**
	 * 车辆所在停车库唯一标识
	 */
	private String parkSyscode;

	/**
	 * 停车库名称
	 */
	private String parkName;

	/**
	 * 父停车库唯一标识
	 */
	private String parentParkSyscode;

	/**
	 * 停车库车位总数
	 */
	private String totalPlace;

	/**
	 * 停车库固定车位总数
	 */
	private String totalPermPlace;

	/**
	 * 停车库预约车位总数
	 */
	private String totalReservePlace;

	/**
	 * 停车库车位剩余数
	 */
	private String leftPlace;

	/**
	 * 停车库固定车位剩余数
	 */
	private String leftPermPlace;

	/**
	 * 停车库预约车位剩余数
	 */
	private String leftReservePlace;
}
