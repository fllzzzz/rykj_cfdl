package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2023/3/28
 */
@Data
@Accessors(chain = true)
public class WhiteListReq extends PageRequest {

	@ApiModelProperty(value = "车牌号")
	private String plateNo;
}
