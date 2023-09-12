package com.cf.parking.facade.dto;

import java.util.Date;

import lombok.Data;
import lombok.experimental.Accessors;


@Data
@Accessors(chain = true)
public class Carmanagement {

	
	/**
	 * 车牌号 必填
	 */
	private String licensePlate;
	/**
	 * 车辆颜色(0:其他颜色/1:白色/2:银色/3:灰色/4:黑色/5:红色/6:深蓝色/7:蓝色/8:黄色/9:绿色/10:棕色/11:粉色/12:紫色/13:深灰/14:杏色/255:未识别的车辆颜色)
	 */
	private String vehicleColor;
	/**
	 * 车辆类型(0:其他车/1:小型车/2:大型车/3:摩托车) 必填
	 */
	private String vehicleType = "1" ;
	/**
	 * (固定/摇号/领导)  传摇号 必填
	 */
	private String carOwnerClassification = "摇号";
	/**
	 * 车场code数组 必填 
	 */
	private String[] parkIndexCode;
	/**
	 * 车主  必填
	 */
	private String carOwner;
	/**
	 * 车主电话
	 */
	private String ownerPhone;
	/**
	 * 工号 必填
	 */
	private String jobNo;
	/**
	 * 权限期限开始时间  yyyy-MM-dd HH:mm:ss  必填
	 */
	private Date permissStart;
	/**
	 * 权限期限结束时间  yyyy-MM-dd HH:mm:ss  必填
	 */
	private Date permissEnd;
	
	
	
}
