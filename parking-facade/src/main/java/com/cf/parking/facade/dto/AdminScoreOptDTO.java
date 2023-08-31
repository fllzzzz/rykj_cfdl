package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author whx
 * @date 2022/10/22
 */
@Data
@Accessors(chain = true)
public class AdminScoreOptDTO {

	private List<Long> userIdList;

	private Integer score;

	private String remark;

	private Long adminUserId;
}
