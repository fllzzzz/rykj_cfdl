package com.cf.parking.services.utils;

import com.cf.support.exception.BusinessException;

public class AssertUtil {

	
	public static void checkNull(Object obj,String message) {
		if (obj == null ) {
			throw new BusinessException(message);
		}
	}

	/**
	 * flag != true时抛出异常
	 * @param flag
	 * @param message
	 */
	public static void checkTrue(Boolean flag, String message) {
		if(flag == null || !flag) {
			throw new BusinessException(message);
		}
	}
}
