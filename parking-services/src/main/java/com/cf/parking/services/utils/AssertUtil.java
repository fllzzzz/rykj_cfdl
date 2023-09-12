package com.cf.parking.services.utils;

import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import com.cf.support.exception.BusinessException;


public class AssertUtil {

	
	public static void checkNull(Object obj,String message) {
		
		
		if (ObjectUtils.isEmpty(obj) ) {
			throw new BusinessException(message);
		}
		
		if (obj instanceof String) {
			if(StringUtils.isEmpty(obj.toString())) {
				throw new BusinessException(message);
			}
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
