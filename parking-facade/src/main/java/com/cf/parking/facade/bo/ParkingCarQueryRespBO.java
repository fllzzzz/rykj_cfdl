package com.cf.parking.facade.bo;

import java.util.List;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 车辆查询出参
 * @author think
 *
 */
@Data
@Accessors(chain = true)
public class ParkingCarQueryRespBO<ParkingCarInfoBO> {

	/**
	 * 状态码 200成功
	 */
	private String code;
	
	private String message;
	
	private List<ParkingCarInfoBO> data;
	
	private Long total;
}
