package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * @author whx
 * @date 2023/3/28
 */
@Data
public class ScheduleDateRsp {

	@ApiModelProperty(value = "排班日期")
	private List<String> shiftDateList;

}
