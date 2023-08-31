package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author whx
 * @date 2023/3/27
 */
@Data
@Accessors(chain = true)
public class ScheduleDataDTO implements Serializable {
	/**
	 * 开始时间(yyyy-MM-dd)
	 */
	private String startDate;

	/**
	 * 结束时间(yyyy-MM-dd)
	 */
	private String endDate;

	/**
	 * 工号
	 */
	private String empId;
}
