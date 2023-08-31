package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2022/11/11
 */
@Data
@Accessors(chain = true)
public class UserBO {

	private Long userId;

	private String jobNumber;

	private String name;

	private String avatar;

	private String mobile;

	/**
	 * title:乘客、车主
	 */
	private String title;
}
