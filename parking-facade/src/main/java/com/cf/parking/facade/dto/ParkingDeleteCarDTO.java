package com.cf.parking.facade.dto;

import java.util.List;

import lombok.Data;

@Data
public class ParkingDeleteCarDTO {

	
	/**
	 * 车辆在海康的id，通过查询接口获取
	 */
	private List<String> id;
}
