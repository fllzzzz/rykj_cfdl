package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;


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
	
	
	@ApiModelProperty(value = "申请人名称，PC端列表查询时用户名称只需要使用这个，不需要交换人名称")
    private String userName;
	
	@ApiModelProperty(value = "状态")
	private String state;

	/** 申请日期（起） */
	@ApiModelProperty(value = "申请日期（起）")
	@JsonFormat(pattern = "yyyy-MM-dd")
	private Date applyStartDate;

	/** 申请日期（止） */
	@ApiModelProperty(value = "申请日期（止）")
	@JsonFormat(pattern = "yyyy-MM-dd")
	private Date applyEndDate;

	/**停车场 */
	@ApiModelProperty(value = "交换车位")
	private String parkingLotCode;

}
