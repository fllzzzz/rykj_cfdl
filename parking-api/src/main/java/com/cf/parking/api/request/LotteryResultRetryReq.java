package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;




@Data
@Accessors(chain = true)
public class LotteryResultRetryReq {
	
	
	/**
	 * 批次id
	 */
	@ApiModelProperty(value = "批次ID")
	private Long batchId;
	
	/**
	 * 轮次id
	 */
	@ApiModelProperty(value = "轮次ID")
	private Long roundId;
}
