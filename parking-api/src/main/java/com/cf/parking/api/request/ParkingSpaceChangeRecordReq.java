package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;


/**
 * 车位交换记录
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "停车场交换查询对象")
public class ParkingSpaceChangeRecordReq extends PageRequest {

    /** userId */
	@ApiModelProperty(value = "申请人或交换人ID")
    private Long userId;
	
	
	@ApiModelProperty(value = "交换人名称")
    private String acceptUserName;
	
	
	@ApiModelProperty(value = "申请人名称")
    private String userName;
	
	@ApiModelProperty(value = "状态")
	private String state;

}