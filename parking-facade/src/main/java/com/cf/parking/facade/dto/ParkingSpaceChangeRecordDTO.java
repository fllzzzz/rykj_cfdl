package com.cf.parking.facade.dto;

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
public class ParkingSpaceChangeRecordDTO extends PageRequest {

    /** userId */
	@ApiModelProperty(value = "申请人或交换人ID")
    private Long userId;
	
	
	@ApiModelProperty(value = "交换人名称")
    private String acceptUserName;
	
	
	@ApiModelProperty(value = "申请人名称")
    private String userName;
	
	@ApiModelProperty(value = "状态")
	private String state;
	
	/**
     * 状态（0：申请，1：已同意，2：已拒绝，3：已撤销）
     */
	@ApiModelProperty(value = "查询不等于nestate的记录")
    private String nestate;

	/** 申请日期（起） */
	@JsonFormat(pattern = "yyyy-MM-dd")
	private Date applyStartDate;

	/** 申请日期（止） */
	@JsonFormat(pattern = "yyyy-MM-dd")
	private Date applyEndDate;

	/**停车场 */
	private String parkingLotCode;

}
