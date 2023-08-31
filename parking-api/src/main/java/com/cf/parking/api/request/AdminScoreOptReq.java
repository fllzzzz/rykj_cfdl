package com.cf.parking.api.request;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author whx
 * @date 2022/10/21
 */
@Data
@Accessors(chain = true)
public class AdminScoreOptReq {
	private List<Long> userIdList;

	private Integer score;

	private String remark;
}
