package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author whx
 * @date 2023/3/29
 */
@Data
@Accessors(chain = true)
public class CrossRecordsQueryDTO implements Serializable {
	/**
	 * 查询开始时间 ISO8601格式： yyyy-MM-ddTHH:mm:ss+当前时区，例如北京时间： 2018-07-26T15:00:00+08:00
	 */
	private String startTime;
	/**
	 * 查询结束时间  ISO8601格式： yyyy-MM-ddTHH:mm:ss+当前时区，例如北京时间： 2018-07-26T15:00:00+08:00
	 */
	private String endTime;

	/**
	 * 目标页码，范围 ( 0 , ~ )
	 */
	private Integer pageNo;

	/**
	 * 每页记录数，范围 ( 0 , 1000 ]
	 */
	private Integer pageSize = 1000;
}
