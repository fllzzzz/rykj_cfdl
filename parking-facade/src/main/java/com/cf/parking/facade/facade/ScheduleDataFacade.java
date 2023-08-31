package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.ScheduleDateBO;

/**
 * @author whx
 * @date 2023/3/27
 */
public interface ScheduleDataFacade {

	/**
	 * 排班数据定时任务
	 */
	void getGaiaAttendance();

	/**
	 * 查询排班数据
	 *
	 * @return
	 */
	ScheduleDateBO getScheduleDate();
}