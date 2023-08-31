package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2023/3/29
 */
@Data
@Accessors(chain = true)
public class WhiteListAddDTO {

	/**
	 * 车牌号文本
	 */
	private String plateNoText;
}
