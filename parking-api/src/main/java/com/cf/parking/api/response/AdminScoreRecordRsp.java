package com.cf.parking.api.response;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2022/10/21
 */
@Data
@Accessors(chain = true)
public class AdminScoreRecordRsp {
	private Long userId;

	private String name;

	private String jobNumber;

	private Integer totalScore;
}
