package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * @author whx
 * @date 2023/3/28
 */
@Data
public class WhiteListBatchDelReq {
	@ApiModelProperty(value = "主键id列表")
	private List<Long> whiteListIds;
}
