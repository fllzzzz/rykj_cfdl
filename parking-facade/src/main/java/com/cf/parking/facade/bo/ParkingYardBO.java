package com.cf.parking.facade.bo;


import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author think
 *	停车场信息
 */
@Data
@Accessors(chain = true)
public class ParkingYardBO {

	/**
	 * 车场名称
	 */
	private String yardName;
	/**
	 * 海康接口返回的停车库唯一标识
	 */
	private String code;
}
