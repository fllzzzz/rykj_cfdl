package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2022/10/22
 */
@Data
@Accessors(chain = true)
public class AdminScoresPageDTO extends PageRequest {
	private String name;

	private String jobNumber;
}