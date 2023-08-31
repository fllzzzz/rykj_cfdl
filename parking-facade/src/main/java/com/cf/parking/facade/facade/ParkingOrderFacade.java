package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.*;
import com.cf.parking.facade.dto.*;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;

import java.util.List;

public interface ParkingOrderFacade {

	/**
	 * 查询订单分页
	 *
	 * @param userId
	 * @param param
	 * @return
	 */
	PageResponse<ParkingOrderBO> getOrderPage(Long userId, RidingOrderDTO param);

	/**
	 * 乘客请求搭车
	 *
	 * @param userId
	 * @param parkingOrderId
	 * @return
	 */
	Result requestConfirmOrder(Long userId, Long parkingOrderId);

	/**
	 * 乘客取消请求搭车
	 *
	 * @param userId
	 * @param parkingOrderId
	 * @return
	 */
	Result requestCancelOrder(Long userId, Long parkingOrderId);

    /**
     * 乘客取消订单
     *
     * @param userId
     * @param parkingOrderId
     * @return
     */
    Result cancelByPassenger(Long userId, Long parkingOrderId);

	/**
	 * 乘客确认下车
	 *
	 * @param userId
	 * @param param
	 * @return
	 */
	Result arriveByPassenger(Long userId, PassengerArriveDTO param);

    /**
	 * 提交订单
	 *
	 * @param parkingOrderDTO
	 * @return
	 */
	Result<OrderCommitBO> commitOrder(ParkingOrderDTO parkingOrderDTO);

	/**
	 * 订单详情
	 *
	 * @param userId
	 * @param parkingOrderId
	 * @return
	 */
	Result<OrderDetailBO> orderDetail(Long userId, Long parkingOrderId);

	/**
	 * 司机未完成订单
	 *
	 * @param userId
	 * @return
	 */
	List<DriverOrderUnfinishedBO> pendingDrive(Long userId);

	/**
	 * 司机确认同行
	 *
	 * @param orderConfirmPeerDTO
	 * @return
     */
    Result orderConfirmPeer(OrderConfirmPeerDTO orderConfirmPeerDTO);

    /**
     * 司机取消订单
     *
     * @param userId
     * @param parkingOrderId
     * @return
	 */
	Result cancelDriver(Long userId, Long parkingOrderId);

	/**
	 * 获取我的订单记录
	 *
	 * @param orderRecordDTO
	 * @return
	 */
	PageResponse<OrderRecordBO> orderRecord(OrderRecordDTO orderRecordDTO);

	/**
	 * 处理超时的订单和即将要开始的订单提醒
	 */
	void dealOutTimeAndStartOrder();

	/**
	 * 乘客确认上车，下车接口权限校验
	 *
	 * @param userId         userId
	 * @param parkingOrderId parkingOrderId
	 */
	void orderPermissionCheck(Long userId, Long parkingOrderId);

}