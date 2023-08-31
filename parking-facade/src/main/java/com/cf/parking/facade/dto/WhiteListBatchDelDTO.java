package com.cf.parking.facade.dto;

import lombok.Data;

import java.util.List;

/**
 * @author whx
 * @date 2023/3/28
 */
@Data
public class WhiteListBatchDelDTO {

	/**
	 * 批量删除的id
	 */
	private List<Long> whiteListIds;
}
