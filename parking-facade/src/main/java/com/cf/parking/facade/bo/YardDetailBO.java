package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
public class YardDetailBO {

	
	/**
	 * 记录id
	 */
	private String id;
	/**
	 * 车场名称
	 */
	private String yardname;
	/**
	 * 创建时间
	 */
	private String creationtime;
	/**
	 * 车位数
	 */
	private String numberOfVehicles;
	/**
	 * 备注
	 */
	private String remarks;
	/**
	 * 海康接口返回的停车库唯一标识
	 */
	private String parkIndexCode;
	
	
}
