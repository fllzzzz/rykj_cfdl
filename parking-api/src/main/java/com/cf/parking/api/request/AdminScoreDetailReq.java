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
public class AdminScoreDetailReq extends PageRequest {
	private Long userId;
}
