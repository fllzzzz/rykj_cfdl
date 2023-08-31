package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2023/3/28
 */
@Data
@Accessors(chain = true)
public class WhiteListPageBO {

	/**
	 * 白名单id
	 */
	private Long whiteListId;

	/**
	 * 车牌号
	 */
	private String plateNo;
}
