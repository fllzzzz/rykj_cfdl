package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;

/**
 * @author whx
 * @date 2022/11/11
 */
@Data
public class UserCommonAddressRsp {
	@ApiModelProperty(value = "常用地址id")
	private Long userCommonAddressId;

	@ApiModelProperty(value = "userId")
	private Long userId;

	@ApiModelProperty(value = "省")
	private String province;

	@ApiModelProperty(value = "市")
	private String city;

	@ApiModelProperty(value = "区")
	private String adName;

	@ApiModelProperty(value = "详细地址")
	private String title;

	@ApiModelProperty(value = "经度")
	private BigDecimal longitude;

	@ApiModelProperty(value = "纬度")
	private BigDecimal latitude;
}
