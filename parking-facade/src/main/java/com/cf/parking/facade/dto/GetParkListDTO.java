package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class GetParkListDTO {
	/**
	 * 停车场唯一标识集合
	 * 多个值使用英文逗号分隔，不超过1000
	 */
	private String parkIndexCodes;
}
