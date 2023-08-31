package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author whx
 * @date 2022/11/11
 */
@Data
public class UserRsp {
	@ApiModelProperty(value = "userId")
	private Long userId;

	@ApiModelProperty(value = "工号")
	private String jobNumber;

	@ApiModelProperty(value = "姓名")
	private String name;

	@ApiModelProperty(value = "头像url")
	private String avatar;

	@ApiModelProperty(value = "手机号")
	private String mobile;

	@ApiModelProperty(value = "title:乘客、车主")
	private String title;
}
