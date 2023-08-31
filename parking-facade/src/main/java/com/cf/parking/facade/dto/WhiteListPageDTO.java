package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2023/3/28
 */
@Data
@Accessors(chain = true)
public class WhiteListPageDTO extends PageRequest {

	/**
	 * 车牌号
	 */
	private String plateNo;
}
