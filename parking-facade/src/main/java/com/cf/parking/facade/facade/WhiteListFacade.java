package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.WhiteListPageBO;
import com.cf.parking.facade.dto.WhiteListBatchDelDTO;
import com.cf.parking.facade.dto.WhiteListAddDTO;
import com.cf.parking.facade.dto.WhiteListPageDTO;
import com.cf.support.result.PageResponse;

/**
 * @author whx
 * @date 2023/3/28
 */
public interface WhiteListFacade {

	/**
	 * 批量删除白名单
	 *
	 * @param param plateNos
	 * @return Result
	 */
	void whiteListBatchDel(WhiteListBatchDelDTO param);

	/**
	 * 分页查询白名单
	 *
	 * @param param param
	 * @return pageResponse
	 */
	PageResponse<WhiteListPageBO> page(WhiteListPageDTO param);

	/**
	 * 批量保存白名单
	 *
	 * @param param
	 */
	void whiteListSave(WhiteListAddDTO param);
}
