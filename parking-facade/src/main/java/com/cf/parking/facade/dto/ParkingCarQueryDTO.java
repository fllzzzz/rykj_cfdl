package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author think
 *	车辆查询入参
 */
@Data
@Accessors(chain = true)
public class ParkingCarQueryDTO {

	/**
	 * 车主名称
	 */
	private String carOwner;
	/**
	 * 车牌号码
	 */
	private String licensePlate;
	/**
	 * 车辆类型
	 */
	private String vehicleType;
	/**
	 *车主分类 (固定/摇号/领导)
	 */
	private String carOwnerClassification = "摇号";
	/**
	 * 页数
	 */
	private Long pageNum = 1l;
	/**
	 * 条数
	 */
	private Long pageSize = 20l;
}
