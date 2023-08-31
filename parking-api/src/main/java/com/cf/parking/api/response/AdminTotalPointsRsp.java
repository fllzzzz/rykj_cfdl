package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2022/10/28
 */
@Data
@Accessors(chain = true)
public class AdminTotalPointsRsp {

	@ApiModelProperty(value = "总积分")
	private Integer totalPoints;

	@ApiModelProperty(value = "已兑换总积分")
	private Integer redeemedPoints;

	@ApiModelProperty(value = "未兑换总积分")
	private Integer unchangedPoints;
}
