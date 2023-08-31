package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.OrderPeerPOMapper;
import com.cf.parking.dao.po.OrderPeerPO;
import com.cf.parking.dao.po.ValidCountPO;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.parking.facade.enums.OrderPeerStateEnum;
import com.cf.parking.services.utils.EmptyUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.exception.BusinessException;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.*;

/**
 * @author lpy
 * @date 2022/10/19
 */

@Service
public class OrderPeerService extends ServiceImpl<OrderPeerPOMapper, OrderPeerPO> implements IService<OrderPeerPO> {
	@Resource
	private IdWorker idWorker;

	/**
	 * 订单详情：根据orderId和state获取同行记录人
	 *
	 * @param parkingOrderId
	 * @return
	 */
	public OrderPeerPO getOrderPeerByState(Long parkingOrderId) {
		LambdaQueryWrapper<OrderPeerPO> orderPeerPOLambdaQueryWrapper = new LambdaQueryWrapper<>();
		orderPeerPOLambdaQueryWrapper
				.eq(OrderPeerPO::getParkingOrderId, parkingOrderId)
				.notIn(OrderPeerPO::getRecordState, Arrays.asList(OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode(),
						OrderPeerStateEnum.ORDER_PEER_CANCEL_REQUEST.getCode(), OrderPeerStateEnum.ORDER_PEER_FAIL.getCode()));
        return this.getOne(orderPeerPOLambdaQueryWrapper);
    }

	@Resource
	public OrderPeerPOMapper orderPeerPOMapper;

	/**
	 * 查询乘客已请求的订单
	 *
	 * @param userId
	 * @return
	 */
	public List<Long> selectRequestOrderList(Long userId) {
		LambdaQueryWrapper<OrderPeerPO> wrapper =
				new LambdaQueryWrapper<OrderPeerPO>()
						.eq(OrderPeerPO::getUserId, userId)
						.eq(OrderPeerPO::getRecordState, OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode());
		List<OrderPeerPO> orderPeerPOS = orderPeerPOMapper.selectList(wrapper);
		List orderIdList = new ArrayList<>();
		for (OrderPeerPO po : orderPeerPOS) {
			orderIdList.add(po.getParkingOrderId());
		}
		return orderIdList;
	}

	/**
	 * 查询乘客未完成的同行记录
	 *
	 * @param userId
	 * @return
	 */
	public List<OrderPeerPO> getPassengerPendingOrder(Long userId) {
		LambdaQueryWrapper<OrderPeerPO> wrapper = new LambdaQueryWrapper<OrderPeerPO>()
				.eq(OrderPeerPO::getUserId, userId)
				.in(OrderPeerPO::getRecordState, OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode(), OrderPeerStateEnum.ORDER_PEER_CONFIRM_RIDING.getCode())
				.orderByAsc(OrderPeerPO::getOrderTime);
		wrapper.last("limit 2");
		return this.list(wrapper);
	}

	/**
	 * 查询同行记录详情
	 *
	 * @param userId
	 * @return
	 */
	public List<OrderPeerPO> selectRecordByUserId(Long userId) {
		LambdaQueryWrapper<OrderPeerPO> wrapper =
				new LambdaQueryWrapper<OrderPeerPO>()
						.eq(OrderPeerPO::getUserId, userId)
						.notIn(OrderPeerPO::getRecordState
								, OrderPeerStateEnum.ORDER_PEER_FAIL.getCode(), OrderPeerStateEnum.ORDER_PEER_CANCEL.getCode());
		return orderPeerPOMapper.selectList(wrapper);
	}

	/**
	 * 查询订单的请求数
	 *
	 * @param parkingOrderId
	 * @return
	 */
	public Long countRecord(Long parkingOrderId) {
		LambdaQueryWrapper<OrderPeerPO> queryWrapper = new LambdaQueryWrapper<OrderPeerPO>()
				.eq(OrderPeerPO::getParkingOrderId, parkingOrderId)
				.eq(OrderPeerPO::getRecordState, OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode());
		return this.count(queryWrapper);
	}

	/**
	 * 更新同行状态
	 *
	 * @param orderPeerPO
	 * @param recordState
	 * @return
	 */
	@Transactional(rollbackFor = Exception.class)
	public void updateState(OrderPeerPO orderPeerPO, Integer recordState) {
		LambdaUpdateWrapper<OrderPeerPO> updateWrapper =
				new LambdaUpdateWrapper<OrderPeerPO>()
						.eq(OrderPeerPO::getOrderPeerId, orderPeerPO.getOrderPeerId())
						.eq(OrderPeerPO::getRecordState, orderPeerPO.getRecordState())
						.set(OrderPeerPO::getRecordState, recordState);
		if (!this.update(updateWrapper)) {
			throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH.getMsg());
		}
	}

	/**
	 * 校验并更新同行状态
	 *
	 * @param orderPeerPO
	 * @param recordState
	 * @return
	 */
	@Transactional(rollbackFor = Exception.class)
	public void updateStateList(OrderPeerPO orderPeerPO, Integer recordState, Collection<Integer> list) {
		LambdaUpdateWrapper<OrderPeerPO> updateWrapper =
				new LambdaUpdateWrapper<OrderPeerPO>()
						.eq(OrderPeerPO::getOrderPeerId, orderPeerPO.getOrderPeerId())
						.in(OrderPeerPO::getRecordState, list)
						.set(OrderPeerPO::getRecordState, recordState);
		if (!this.update(updateWrapper)) {
			throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH.getMsg());
		}
	}

	/**
	 * 更新同行状态
	 *
	 * @param orderPeerPO
	 * @param recordState
	 * @return
	 */
	@Transactional(rollbackFor = Exception.class)
	public void updateStateLongLatAndDis(OrderPeerPO orderPeerPO, Integer recordState) {
		LambdaUpdateWrapper<OrderPeerPO> updateWrapper =
				new LambdaUpdateWrapper<OrderPeerPO>()
						.eq(OrderPeerPO::getOrderPeerId, orderPeerPO.getOrderPeerId())
						.eq(OrderPeerPO::getRecordState, orderPeerPO.getRecordState())
						.set(OrderPeerPO::getRecordState, recordState)
						.set(ObjectUtils.isNotEmpty(orderPeerPO.getDestDistance()), OrderPeerPO::getDestDistance, orderPeerPO.getDestDistance())
						.set(ObjectUtils.isNotEmpty(orderPeerPO.getStartDestDistance()), OrderPeerPO::getStartDestDistance, orderPeerPO.getStartDestDistance())
						.set(ObjectUtils.isNotEmpty(orderPeerPO.getPassengerDestLongitude()), OrderPeerPO::getPassengerDestLongitude, orderPeerPO.getPassengerDestLongitude())
						.set(ObjectUtils.isNotEmpty(orderPeerPO.getPassengerDestLatitude()), OrderPeerPO::getPassengerDestLatitude, orderPeerPO.getPassengerDestLatitude())
						.set(ObjectUtils.isNotEmpty(orderPeerPO.getGetOffTm()), OrderPeerPO::getGetOffTm, orderPeerPO.getGetOffTm());
		if (!this.update(updateWrapper)) {
			throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH.getMsg());
		}
	}

	/**
	 * 插入一条同行记录
	 *
	 * @param orderPeerPO
	 * @return
	 */
	public int insertOrderPeer(OrderPeerPO orderPeerPO) {
		orderPeerPO.setOrderPeerId(idWorker.nextId()).setRecordState(OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode());
		return orderPeerPOMapper.insertSelective(orderPeerPO);
	}

	public OrderPeerPO selectOne(Long parkingOrderId, Long userId) {
		LambdaQueryWrapper<OrderPeerPO> wrapper =
				new LambdaQueryWrapper<OrderPeerPO>()
						.eq(OrderPeerPO::getParkingOrderId, parkingOrderId)
						.eq(OrderPeerPO::getUserId, userId);
		OrderPeerPO orderPeerPO = orderPeerPOMapper.selectOne(wrapper);
		if (ObjectUtils.isEmpty(orderPeerPO)) {
			EmptyUtils.emptyPeerOrder(userId);
		}
		return orderPeerPO;
	}

	/**
	 * 司机确认同行：根据peerId获取同行记录
	 *
	 * @param orderPeerId
	 * @return
	 */
	public OrderPeerPO getOrderPeerByPeerId(Long orderPeerId) {
		return this.getById(orderPeerId);
	}

	/**
	 * 司机确认同行：根据peerId更新state
	 *
	 * @param orderPeerId
	 * @return
	 */
	public Boolean updateOrderPeerStateByPeerId(Long orderPeerId) {
		//同行表的当前人记录状态改成 已确认
		LambdaUpdateWrapper<OrderPeerPO> updateWrapper =
				new LambdaUpdateWrapper<OrderPeerPO>()
						.eq(OrderPeerPO::getOrderPeerId, orderPeerId)
						.set(OrderPeerPO::getRecordState, OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode());
		return this.update(updateWrapper);
    }

    /**
     * 司机取消订单，更改未开始订单为未成单
     *
     * @param parkingOrderId
     * @return
     */
    public Boolean updateOtherPeerStateByPeerId(Long orderPeerId, Long parkingOrderId) {
        // 同行表的其他人记录状态改成 未成单
        LambdaUpdateWrapper<OrderPeerPO> peerFailWrapper =
                new LambdaUpdateWrapper<OrderPeerPO>()
                        .ne(ObjectUtils.isNotEmpty(orderPeerId), OrderPeerPO::getOrderPeerId, orderPeerId)
                        .eq(OrderPeerPO::getParkingOrderId, parkingOrderId)
                        .set(OrderPeerPO::getRecordState, OrderPeerStateEnum.ORDER_PEER_FAIL.getCode());
        return this.update(peerFailWrapper);
    }

    /**
     * 司机取消订单，更改未开始订单为未成单
     *
     * @param parkingOrderId
	 * @return
	 */
	public Boolean orderFail(Long parkingOrderId) {
		LambdaUpdateWrapper<OrderPeerPO> updateWrapper =
				new LambdaUpdateWrapper<OrderPeerPO>()
						.eq(OrderPeerPO::getParkingOrderId, parkingOrderId)
						.set(OrderPeerPO::getRecordState, OrderPeerStateEnum.ORDER_PEER_FAIL.getCode());
		return this.update(updateWrapper);
	}


	/**
	 * 根据userId和状态获取同行记录
	 *
	 * @param userId
	 * @return
	 */
	public List<OrderPeerPO> queryOrderPeerListByUserIdAndState(Long userId, List<Integer> state) {
		LambdaQueryWrapper<OrderPeerPO> queryWrapper =
				new LambdaQueryWrapper<OrderPeerPO>()
						.eq(OrderPeerPO::getUserId, userId)
						.notIn(OrderPeerPO::getRecordState, state);
		return this.list(queryWrapper);
	}

	/**
	 * 根据orderId和通行记录表中recordState查询同行记录表
	 * 获取已请求的记录
	 *
	 * @param parkingOrderId
	 * @return
	 */
	public List<OrderPeerPO> getRequestedList(Long parkingOrderId) {
		LambdaQueryWrapper<OrderPeerPO> queryWrapper = new LambdaQueryWrapper<OrderPeerPO>()
				.eq(OrderPeerPO::getParkingOrderId, parkingOrderId).eq(OrderPeerPO::getRecordState, OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode());
		// 打车列表
		return this.list(queryWrapper);
	}

	/**
	 * 后台根据orderId  状态不等于已请求，取消请求和未成单
	 *
	 * @param ids
	 * @return
	 */
	public List<OrderPeerPO> getOrderPeerByStateAndOrderIds(List<Long> ids) {
		LambdaQueryWrapper<OrderPeerPO> wrapper = new LambdaQueryWrapper<OrderPeerPO>()
				.in(OrderPeerPO::getParkingOrderId, ids)
				.notIn(OrderPeerPO::getRecordState, Arrays.asList(OrderPeerStateEnum.ORDER_PEER_FAIL.getCode(),
						OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode(), OrderPeerStateEnum.ORDER_PEER_CANCEL_REQUEST.getCode()));
        return this.list(wrapper);
    }

    /**
     * 司机评价：据 乘客userId 和 订单号 查询同行记录表
     *
     * @return
     */
    public OrderPeerPO queryPeerRecordByUserIdAndOrderId(Long userId, Long parkingOrderId) {
        LambdaQueryWrapper<OrderPeerPO> wrapper =
                new LambdaQueryWrapper<OrderPeerPO>()
                        .eq(OrderPeerPO::getUserId, userId)
                        .eq(OrderPeerPO::getParkingOrderId, parkingOrderId);
        return this.getOne(wrapper);
	}

	/**
	 * 根据订单id获取指定订单状态的列表
	 *
	 * @param orderIdList 订单列表
	 * @return 同行列表
	 */
	public List<OrderPeerPO> getOrderPeerByOrderListAndStatusList(List<Long> orderIdList, List<Integer> stateList) {
		return list(new LambdaQueryWrapper<OrderPeerPO>()
				.in(OrderPeerPO::getParkingOrderId, orderIdList)
				.in(CollectionUtils.isNotEmpty(stateList), OrderPeerPO::getRecordState, stateList));
	}

	/**
	 * 根据orderPeerIdList更新状态
	 *
	 * @param orderPeerIdList
	 * @param state
	 * @return
	 */
	public Boolean updateOrderPeerByOrderPeerIdList(List<Long> orderPeerIdList, Integer state) {
		LambdaUpdateWrapper<OrderPeerPO> wrapper = new LambdaUpdateWrapper<OrderPeerPO>()
				.in(OrderPeerPO::getOrderPeerId, orderPeerIdList)
				.set(OrderPeerPO::getRecordState, state);
		return this.update(wrapper);
	}

	public Long getOrderPeerByOrderIdAndUserId(Long orderId, Long userId) {
		return this.count(
				new LambdaQueryWrapper<OrderPeerPO>()
						.eq(OrderPeerPO::getParkingOrderId, orderId)
						.eq(OrderPeerPO::getUserId, userId));
	}

	/**
	 * 根据orderId +userId 获取同行人
	 *
	 * @param orderId orderId
	 * @param userId  userId
	 * @return
	 */
	public OrderPeerPO getPeerByOrderIdAndUserId(Long orderId, Long userId) {
		return this.getOne(
				new LambdaQueryWrapper<OrderPeerPO>()
						.eq(OrderPeerPO::getParkingOrderId, orderId)
						.eq(OrderPeerPO::getUserId, userId));
	}

	/**
	 * 根据userId ,状态，开始结束时间来判断是否该乘客被其他司机确认同行  已确认+已结束 新增 乘客确认上车
	 *
	 * @param userId    userId
	 * @param beginTime 开始时间
	 * @param endTime   结束时间
	 * @return count
	 */
	public Long getUserConfirmedByOtherDriver(Long userId, Date beginTime, Date endTime) {
		return this.count(new LambdaQueryWrapper<OrderPeerPO>()
				.eq(OrderPeerPO::getUserId, userId)
				.in(OrderPeerPO::getRecordState, OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode(), OrderPeerStateEnum.ORDER_PEER_COMPLETE.getCode(), OrderPeerStateEnum.ORDER_PEER_CONFIRM_RIDING.getCode())
				.between(OrderPeerPO::getOrderTime, beginTime, endTime));
	}

	/**
	 * 判断同行记录距离 经纬度 和状态并更新通行记录
	 *
	 * @param orderPeerPO
	 * @return
	 */
	public Boolean updateDistance(OrderPeerPO orderPeerPO) {
		return this.update(
				new LambdaUpdateWrapper<OrderPeerPO>()
						.eq(OrderPeerPO::getParkingOrderId, orderPeerPO.getParkingOrderId())
						.eq(OrderPeerPO::getUserId, orderPeerPO.getUserId())
						.set(ObjectUtils.isNotEmpty(orderPeerPO.getStartDistance()), OrderPeerPO::getStartDistance, orderPeerPO.getStartDistance())
						.set(ObjectUtils.isNotEmpty(orderPeerPO.getRecordState()), OrderPeerPO::getRecordState, orderPeerPO.getRecordState())
						.set(ObjectUtils.isNotEmpty(orderPeerPO.getPassengerStartLongitude()), OrderPeerPO::getPassengerStartLongitude, orderPeerPO.getPassengerStartLongitude())
						.set(ObjectUtils.isNotEmpty(orderPeerPO.getPassengerStartLatitude()), OrderPeerPO::getPassengerStartLatitude, orderPeerPO.getPassengerStartLatitude())
						.set(ObjectUtils.isNotEmpty(orderPeerPO.getGetInTm()), OrderPeerPO::getGetInTm, orderPeerPO.getGetInTm()));
	}

	/**
	 * 查询时间范围内指定状态的单数
	 *
	 * @param beforeData 起始时间
	 * @param afterData  结束时间
	 * @param status     订单状态
	 * @return 根据人分类的单数
	 */
	public List<ValidCountPO> getValidCount(Date beforeData, Date afterData, Integer status) {
		return orderPeerPOMapper.getValidCount(beforeData, afterData, status);
	}
}
