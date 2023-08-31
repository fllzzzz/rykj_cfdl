package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.PendingBO;
import com.cf.parking.facade.dto.CheckDTO;
import com.cf.parking.facade.dto.ConfirmRidingDTO;
import com.cf.support.result.Result;

import java.util.List;

public interface OrderPeerFacade {

	/**
	 * 查询乘客未完成订单
	 *
	 * @param userId
	 * @return
	 */
	List<PendingBO> getPassengerPendingOrder(Long userId);

	/**
	 * 提醒未确认的订单
	 */
	void noticeNoSureOrder();

	/**
	 * 乘客确认上车
	 *
	 * @param dto 前端传进来的起始点经纬度
	 * @return Result
	 */
	Result confirmRiding(ConfirmRidingDTO dto);

	/**
	 * 乘客确认上车确认下车接口校验
	 *
	 * @param dto    校验参数数据传输对象
	 * @param userId userId
	 * @return result
	 */
	Result getOnCheck(Long userId, CheckDTO dto);

	/**
	 * 乘客确认下车接口校验
	 *
	 * @param userId userId
	 * @param dto    dto
	 * @return result
	 */
	Result getOffCheck(Long userId, CheckDTO dto);

}
