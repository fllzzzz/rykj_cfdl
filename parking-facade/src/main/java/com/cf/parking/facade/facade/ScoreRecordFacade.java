package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.AdminScoreRecordBO;
import com.cf.parking.facade.bo.ScoreRecordBO;
import com.cf.support.result.PageRequest;
import com.cf.support.result.PageResponse;

import java.util.Date;
import java.util.List;

public interface ScoreRecordFacade {

	/**
	 * 查询积分记录分页
	 *
	 * @param userId
	 * @param param
	 * @return
	 */
	PageResponse<ScoreRecordBO> getScoresPage(Long userId, PageRequest param);

	/**
	 * 导出时间段内积分排行
	 *
	 * @param beginDate
	 * @param endDate
	 * @return
	 */
	List<AdminScoreRecordBO> exportTimeScore(Date beginDate, Date endDate);
}
