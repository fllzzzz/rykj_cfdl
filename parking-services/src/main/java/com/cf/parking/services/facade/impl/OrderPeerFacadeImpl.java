package com.cf.parking.services.facade.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.cf.parking.dao.po.OrderPeerPO;
import com.cf.parking.dao.po.OrderStateRecordPO;
import com.cf.parking.dao.po.ParkingOrderPO;
import com.cf.parking.dao.po.UserPO;
import com.cf.parking.facade.bo.PendingBO;
import com.cf.parking.facade.constant.MessageConstant;
import com.cf.parking.facade.dto.CardMessageDTO;
import com.cf.parking.facade.dto.CheckDTO;
import com.cf.parking.facade.dto.ConfirmRidingDTO;
import com.cf.parking.facade.enums.*;
import com.cf.parking.facade.facade.DingTalkMessageFacade;
import com.cf.parking.facade.facade.OrderPeerFacade;
import com.cf.parking.services.properties.DingTalkProperties;
import com.cf.parking.services.service.OrderPeerService;
import com.cf.parking.services.service.OrderStateRecordService;
import com.cf.parking.services.service.ParkingOrderService;
import com.cf.parking.services.service.UserService;
import com.cf.parking.services.utils.EmptyUtils;
import com.cf.parking.services.utils.LongLatitudeUtils;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.Result;
import com.cf.support.utils.CFDateUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author whx
 * @date 2022/10/20
 */
@Service
@Slf4j
public class OrderPeerFacadeImpl implements OrderPeerFacade {
	/**
	 * 校验最大距离
	 */
	private final Double MAX_CHECK_DISTANCE = 0.5;
	@Resource
	private OrderPeerService orderPeerService;
	@Resource
	private DingTalkProperties dingTalkProperties;
	@Resource
	private UserService userService;
	@Resource
	private ParkingOrderService parkingOrderService;
	@Resource
	private OrderStateRecordService orderStateRecordService;
	@Resource
	private DingTalkMessageFacade dingTalkMessageFacade;

	@Override
	public List<PendingBO> getPassengerPendingOrder(Long userId) {
		List<OrderPeerPO> orderPeerPOS = orderPeerService.getPassengerPendingOrder(userId);
		Map<Long, OrderPeerPO> map = orderPeerPOS.stream().collect(Collectors.toMap(OrderPeerPO::getParkingOrderId, item -> item));
		List<Long> ids = orderPeerPOS.stream().map(OrderPeerPO::getParkingOrderId).collect(Collectors.toList());
		if (CollectionUtils.isEmpty(ids)) {
			return new ArrayList<>();
		}
		List<ParkingOrderPO> orders = parkingOrderService.getOrderListByOrderIds(ids);
		return orders.stream().map(item -> {
			String startAddress = item.getStartCounty() + item.getStartAddress();
			String destAddress = item.getDestCounty() + item.getDestAddress();
			Date orderTime = map.get(item.getParkingOrderId()).getOrderTime();
			return new PendingBO()
					.setOrderTime(orderTime)
					.setParkingOrderId(item.getParkingOrderId())
					.setDestAddress(destAddress)
					// 已过出发时间显示，正在前往目的地,   未到出发时间显示 等待车主出发
					.setOrderStateMsg(orderTime.before(new Date()) ? "正在前往目的地" : "等待车主出发")
					.setOrderTimeShow(CFDateUtils.getDateShow(orderTime))
					.setStartAddress(startAddress);
		}).collect(Collectors.toList());
	}

	@Override
	public void noticeNoSureOrder() {
		List<OrderPeerPO> orderPeerPOList = orderPeerService.list(new LambdaQueryWrapper<OrderPeerPO>()
				.in(OrderPeerPO::getRecordState, Arrays.asList(OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode()
						,OrderPeerStateEnum.ORDER_PEER_CONFIRM_RIDING.getCode()))
				.le(OrderPeerPO::getOrderTime, new Date()));
		if (CollectionUtils.isEmpty(orderPeerPOList)) {
			return;
		}
		List<Long> userIdList = orderPeerPOList.stream().map(OrderPeerPO::getUserId).collect(Collectors.toList());
		List<UserPO> userPOList = userService.getUserByUserIdList(userIdList);
		List<CardMessageDTO> linkMessageDTOList = orderPeerPOList.stream().map(order -> {
			Optional<UserPO> userPOOptional = userPOList.stream().
					filter(userPO -> userPO.getUserId().equals(order.getUserId())).findAny();
			if (userPOOptional.isPresent()) {
				UserPO userPO = userPOOptional.get();
				return new CardMessageDTO()
						.setOpenIdList(Collections.singletonList(userPO.getOpenId())).
								setMessage(MessageConstant.getArriveDestinationMessage())
						.setUrl(dingTalkProperties.getOrderDetailUrl(order.getParkingOrderId()));
			}
			return null;
		}).filter(ObjectUtils::isNotEmpty).collect(Collectors.toList());
		dingTalkMessageFacade.asyncSendBatchCard(linkMessageDTOList, MessageConstant.PARKING_TITLE);
	}

	@Override
	@Transactional(rollbackFor = Exception.class)
	public Result confirmRiding(ConfirmRidingDTO dto) {
		// 校验
		Long orderId = dto.getParkingOrderId();
		Long userId = dto.getUserId();
		ParkingOrderPO order = parkingOrderService.getOrderByOrderId(orderId);
		OrderPeerPO orderPeer = orderPeerService.getPeerByOrderIdAndUserId(orderId, userId);

		this.confirmRidingCheck(order, orderId, userId, orderPeer);

		// 更新同行表 距离+状态+上车时间
		Double distance = LongLatitudeUtils.getDistance(dto.getPassengerStartLongitude(), dto.getPassengerStartLatitude(), order.getStartLongitude(), order.getStartLatitude());
		orderPeer.setStartDistance(BigDecimal.valueOf(distance)).setRecordState(OrderPeerStateEnum.ORDER_PEER_CONFIRM_RIDING.getCode()).setPassengerStartLongitude(dto.getPassengerStartLongitude()).setPassengerStartLatitude(dto.getPassengerStartLatitude()).setGetInTm(new Date());
		if (!orderPeerService.updateDistance(orderPeer)) {
			throw new BusinessException(BizResultCodeEnum.OTHER_SYSTEM_ERROR.getMsg());
		}
		log.info("乘客确认上车 distance: 乘客 userId:{}, 距离起始点距离：{}, orderId:{}", userId, distance, orderId);
		// 更新订单状态记录表
		OrderStateRecordPO orderStateRecordPO = new OrderStateRecordPO();
		orderStateRecordPO.setUserType(UserTypeEnum.PASSENGER.getCode()).setUserId(userId).setOptType(OptTypeEnum.PASSENGER_CONFIRM_RIDING.getCode()).setParkingOrderId(orderId);
		orderStateRecordService.save(orderStateRecordPO);
		return Result.buildSuccessResult();
	}

	@Override
	public Result getOnCheck(Long userId, CheckDTO dto) {
		ParkingOrderPO order = parkingOrderService.getOrderByOrderId(dto.getParkingOrderId());
		EmptyUtils.checkOrder(order, dto.getParkingOrderId(), "确认上车接口");
		Double distance = LongLatitudeUtils.getDistance(dto.getLongitude(), dto.getLatitude(), order.getStartLongitude(), order.getStartLatitude());
		log.info("getOnCheck--乘客确认上车check  乘客 userId:{}, 距离：{}", userId, distance);
		// 这里就不用封装了
		if (distance > MAX_CHECK_DISTANCE) {
			return Result.buildErrorResult("离出发地超过" + distance + "km, 是否确认上车?");
		}
		// 出发时间未到
		if (order.getOrderTime().after(new Date())) {
			return Result.buildResult(BizResultCodeEnum.DEPARTURE_TIME_NOT_ARRIVE);
		}
		return Result.buildSuccessResult();
	}

	@Override
	public Result getOffCheck(Long userId, CheckDTO dto) {
		ParkingOrderPO order = parkingOrderService.getOrderByOrderId(dto.getParkingOrderId());
		EmptyUtils.checkOrder(order, dto.getParkingOrderId(), "确认下车接口");
		Double distance = LongLatitudeUtils.getDistance(dto.getLongitude(), dto.getLatitude(), order.getDestLongitude(), order.getDestLatitude());
		log.info("getOffCheck--乘客确认下车check  乘客 userId:{}, 距离：{}", userId, distance);
		// 距离目的地超过0.5km 返回错误code
		if (distance > MAX_CHECK_DISTANCE) {
			return Result.buildErrorResult("离目的地超过" + distance + "km, 是否确认到达目的地?");
		}
		return Result.buildSuccessResult();
	}

	/**
	 * 确认上车校验
	 *
	 * @param order     订单
	 * @param orderId   订单号
	 * @param userId    userId
	 * @param orderPeer orderPeer
	 */
	private void confirmRidingCheck(ParkingOrderPO order, Long orderId, Long userId, OrderPeerPO orderPeer) {
		// 根据orderId查询当前订单状态是否为进行中
		if (ObjectUtils.isEmpty(order)) {
			EmptyUtils.checkOrder(order, orderId, "乘客确认上车");
		}
		if (!OrderStateEnum.ORDER_ING.getCode().equals(order.getOrderState())) {
			// todo: 文案
			throw new BusinessException(BizResultCodeEnum.ORDER_OPT_FAIL.getMsg());
		}
		// 根据orderId+userId查询同行表该乘客是否为已确认
		if (ObjectUtils.isEmpty(orderPeer)) {
			log.error("没有该同行记录: orderId:{} userId:{}", orderId, userId);
			throw new BusinessException("没有该同行记录");
		}
		if (!OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode().equals(orderPeer.getRecordState())) {
			throw new BusinessException(BizResultCodeEnum.ORDER_PEER_ORDER_STATE_MATCH.getMsg());
		}
	}

}
