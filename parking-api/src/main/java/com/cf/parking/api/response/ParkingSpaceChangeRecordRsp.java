package com.cf.parking.api.response;

import java.util.Date;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;


/**
 * @author think
 *
 */
@Data
@Accessors(chain = true)
public class ParkingSpaceChangeRecordRsp {
    
	@ApiModelProperty(value = "id")
	private Long id;

    /**
     * 申请人停车场
     */
	@ApiModelProperty(value = "申请人停车场")
    private String parkingCode;

    /**
     * 申请人ID
     */
	@ApiModelProperty(value = "申请人ID")
    private Long userId;

    /**
     * 申请人名称
     */
	@ApiModelProperty(value = "申请人名称")
    private String userName;

    /**
     * 交换人ID
     */
	@ApiModelProperty(value = "交换人ID")
    private Long acceptUserId;

    /**
     * 交换人名称
     */
	@ApiModelProperty(value = "交换人名称")
    private String acceptUserName;

    /**
     * 交换人车库
     */
	@ApiModelProperty(value = "交换人车库")
    private String acceptParkingCode;

    /**
     * 车位有效期开始日期
     */
	@ApiModelProperty(value = "车位有效期开始日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validStartDate;

    /**
     * 车位有效期结束日期
     */
	@ApiModelProperty(value = "车位有效期结束日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validEndDate;

    /**
     * 状态（0：申请，1：已同意，2：已拒绝，3：已撤销）
     */
	@ApiModelProperty(value = "状态")
    private String state;

    /**
     * 创建日期
     */
	@ApiModelProperty(value = "创建日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date createTm;

    /**
     * 修改日期
     */
	@ApiModelProperty(value = "修改日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date updateTm;

	@ApiModelProperty(value = "申请人车库名")
	private String parkingName;
	
	@ApiModelProperty(value = "交换人车库名")
	private String acceptParkingName;
}