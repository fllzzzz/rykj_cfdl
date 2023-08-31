package com.cf.parking.services.utils;

import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.support.exception.BusinessException;

import java.util.Collection;

/**
 * @author whx
 * @date 2022/11/2
 */
public class StateUtils {
	/**
	 * 确认订单状态
	 *
	 * @param orderState
	 * @param state
	 */
	public static void orderStateCheck(Integer orderState, Integer state) {
		if (!orderState.equals(state)) {
			throw new BusinessException(BizResultCodeEnum.ORDER_STATE_ERROR.getMsg());
		}
	}

	/**
	 * 确认同行记录状态
	 *
	 * @param recordState
	 * @param state
	 */
	public static void recordStateCheck(Integer recordState, Integer state) {
		if (!recordState.equals(state)) {
			throw new BusinessException(BizResultCodeEnum.ORDER_OPT_FAIL.getMsg());
		}
	}

	/**
	 * 状态判断
	 *
	 * @param recordState state
	 * @param states      状态列表states
	 */
	public static void recordStateListCheck(Integer recordState, Collection<Integer> states) {
		if (states.stream().noneMatch(recordState::equals)) {
			throw new BusinessException(BizResultCodeEnum.ORDER_OPT_FAIL.getMsg());
		}
	}
}
