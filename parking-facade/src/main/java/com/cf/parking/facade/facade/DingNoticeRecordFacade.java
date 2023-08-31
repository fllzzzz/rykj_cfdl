package com.cf.parking.facade.facade;

import com.cf.parking.facade.dto.DingNoticeRecordDTO;

import java.util.List;

/**
 * @author whx
 * @date 2023/3/31
 */
public interface DingNoticeRecordFacade {

	/**
	 * 钉钉通知
	 *
	 * @param param
	 */
	void dingNotify(List<DingNoticeRecordDTO> param);

}
