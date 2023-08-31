package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2022/10/21
 */
@Data
@Accessors(chain = true)
public class AdminScoreRecordBO {
	private Long userId;

	private String name;

	private String jobNumber;

	private Integer totalScore;
}
