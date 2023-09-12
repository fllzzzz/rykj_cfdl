package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
public class QueryYardDTO {
	
	/**
	 * 页数  必填
	 */
	private Long pageNum = 1l;
	/**
	 * 条数 必填
	 */
	private Long pageSize = 10l ;
	
	
	/**
	 *  车场名称
	 */
	private String yardname;
}
