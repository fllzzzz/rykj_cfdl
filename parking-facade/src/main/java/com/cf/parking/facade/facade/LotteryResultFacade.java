package com.cf.parking.facade.facade;


import com.cf.parking.facade.bo.LotteryResultBO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.bo.UserSpaceBO;
import com.cf.parking.facade.dto.LotteryResultDTO;
import com.cf.parking.facade.dto.UserSpaceDTO;
import com.cf.support.result.PageResponse;

/**
 * 摇号结果Service接口
 * 
 * @author
 * @date 2023-09-05
 */
public interface LotteryResultFacade
{

	/**
	 * 摇号
	 * @param id
	 */
	void lottery(Long id);

	/**
	 * 查询摇号结果列表
	 * @param dto
	 * @return
	 */
    PageResponse<LotteryResultBO> getLotteryResultList(LotteryResultDTO dto);

	/**
	 * 结果归档
	 *
	 * @param id
	 * @return
	 */
	Integer archive(Long id);

	/**
	 * 摇号结果确认
	 * @param id
	 */
	void confirm(Long id);
	
	/**
	 * 摇号结果分页查询
	 * @param dto
	 * @return
	 */
	PageResponse<LotteryResultDetailBO> lotteryResult(LotteryResultDTO dto);

	/**
	 * 确认结果查询（用户车位表中的记录）
	 * @param dto
	 * @return
	 */
    PageResponse<LotteryResultDetailBO> confirmResult(UserSpaceDTO dto);
}
