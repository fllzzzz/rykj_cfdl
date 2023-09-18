package com.cf.parking.facade.bo;


import java.util.List;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author think
 *	车辆信息
 */
@Data
@Accessors(chain = true)
public class ParkingCarInfoBO<ParkingYardBO> {

	
	/**
	 * 记录id
	 */
	private String id;
	/**
	 * 车主
	 */
	private String carOwner;
	/**
	 * 车牌号
	 */
	private String licensePlate;
	/**
	 * 车主电话
	 */
	private String ownerPhone;
	/**
	 * 权限开始时间
	 */
	private String permissStart;
	/**
	 * 权限结束时间
	 */
	private String permissEnd;
	/**
	 * 车辆类型
	 */
	private String vehicleType;
	/**
	 * 工号
	 */
	private String jobNo;
	/**
	 * 车辆颜色
	 */
	private String vehicleColor;
	/**
	 * 车主类型
	 */
	private String carOwnerClassification;
	/**
	 * 车场
	 */
	private List<ParkingYardBO> yard;
}
