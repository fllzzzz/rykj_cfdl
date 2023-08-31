package com.cf.parking.services.utils;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import com.cf.parking.dao.po.OrderPeerPO;
import com.cf.parking.dao.po.ParkingOrderPO;
import com.cf.parking.dao.po.UserProfilePO;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.support.exception.BusinessException;
import com.cf.support.utils.DingAlarmUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;

import java.util.Collection;
import java.util.Date;

/**
 * @author whx
 * @date 2022/10/27
 */
@Slf4j
public class EmptyUtils {
	public static void emptyOrder(Long parkingOrderId, Long userId) {
		log.error(parkingOrderId + " 订单查询异常, userId为" + userId);
		DingAlarmUtils.alarmException(parkingOrderId + " 订单查询异常, uid=" + userId);
		throw new BusinessException(BizResultCodeEnum.NO_ORDER.getMsg());
	}

	public static void emptyPeerOrder(Long userId) {
		log.error("同行记录查询异常, userId为{}", userId);
		DingAlarmUtils.alarmException("同行记录查询异常, uid=" + userId);
		throw new BusinessException(BizResultCodeEnum.ORDER_PEER_NULL.getMsg());
	}

	public static void userUnExit(String title, Long userId) {
		String msg = String.format("%s接口异常：不存在userId为%d的用户", title, userId);
		log.error(msg);
		DingAlarmUtils.alarmException(msg);
		throw new BusinessException(BizResultCodeEnum.USER_UN_EXSIT.getMsg());
	}

	public static void emptyOrder(String title, Long orderId) {
		String msg = String.format("%s接口异常：不存在orderId为%d的订单", title, orderId);
		log.error(msg);
		DingAlarmUtils.alarmException(msg);
		throw new BusinessException(BizResultCodeEnum.NO_ORDER.getMsg());
	}

	/**
	 * 检查是否有订单
	 *
	 * @param driver         订单
	 * @param parkingOrderId orderId
	 * @param title          标题
	 */
	public static void checkOrder(ParkingOrderPO driver, Long parkingOrderId, String title) {
		if (ObjectUtils.isEmpty(driver)) {
			// 报警
			emptyOrder(title, parkingOrderId);
		}
	}

	/**
	 * 检查是否有用户
	 *
	 * @param driver 用户
	 * @param userId userId
	 * @param title  标题
	 */
	public static void checkProfile(UserProfilePO driver, Long userId, String title) {
		if (ObjectUtils.isEmpty(driver)) {
			// 报警
			userUnExit(title, userId);
		}
	}

	/**
	 * 订单号为空判断
	 *
	 * @param parkingOrderId
	 */
	public static void emptyParkingOrderId(Long parkingOrderId) {
		if (ObjectUtils.isEmpty(parkingOrderId) || parkingOrderId == 0) {
			throw new BusinessException(BizResultCodeEnum.PARAM_NULL.getMsg());
		}
	}

	public static void emptyLevel(Integer level) {
		if (level < 1 && level > 5) {
			throw new BusinessException(BizResultCodeEnum.PARAM_NULL.getMsg());
		}
	}

	/**
	 * 取消操作时间检查 十分钟之内可以取消
	 *
	 * @param collect 同行实体集合
	 */
	public static void cancelTimeCheck(Collection<OrderPeerPO> collect) {
		collect.forEach(o -> {
			Date getInTm = o.getGetInTm();
			if (ObjectUtils.isEmpty(getInTm)) {
				return;
			}
			DateTime offsetTm = DateUtil.offsetMinute(getInTm, 10);
			if (offsetTm.before(new Date())) {
				throw new BusinessException("超过确认上车十分钟后无法取消");
			}
		});

	}
}

