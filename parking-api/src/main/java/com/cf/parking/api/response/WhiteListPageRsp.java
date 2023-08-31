package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2023/3/28
 */
@Data
@Accessors(chain = true)
public class WhiteListPageRsp {

	@ApiModelProperty(value = "白名单id")
	private Long whiteListId;

	@ApiModelProperty(value = "车牌号")
	private String plateNo;

}
