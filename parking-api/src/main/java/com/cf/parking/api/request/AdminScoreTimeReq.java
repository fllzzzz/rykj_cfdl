package com.cf.parking.api.request;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author whx
 * @date 2022/11/15
 */
@Data
@Accessors(chain = true)
public class AdminScoreTimeReq {
	private Date beginDate;

	private Date endDate;
}
