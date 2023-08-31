package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotBlank;

/**
 * @author whx
 * @date 2023/3/29
 */
@Data
@ApiModel(value = "白名单新增实体", description = "白名单新增实体")
public class WhiteListAddReq {

	@ApiModelProperty(value = "车牌号文本")
	@NotBlank(message = "车牌号不能为空")
	private String plateNoText;
}
