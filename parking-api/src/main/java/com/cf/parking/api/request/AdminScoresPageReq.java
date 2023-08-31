package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2022/10/22
 */
@Data
@Accessors(chain = true)
public class AdminScoresPageReq extends PageRequest {
	private String name;

	private String jobNumber;
}
