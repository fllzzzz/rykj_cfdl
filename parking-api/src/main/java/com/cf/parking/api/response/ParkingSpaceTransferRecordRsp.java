package com.cf.parking.api.response;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 车位转赠记录
 * @author
 * @date 2023/09/05
 */
@Data
@ApiModel(description = "停车场查询结果")
public class ParkingSpaceTransferRecordRsp {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** 转赠停车场编号 */
    @ApiModelProperty(value = "转赠停车场编号")
    private String parkingLotCode;

    /** 转赠停车场名称 */
    @ApiModelProperty(value = "转赠停车场名称")
    private String parkingLotRegion;

    /** 申请人userId */
    @ApiModelProperty(value = "申请人userId")
    private Long userId;

    /** 赠予人姓名 */
    @ApiModelProperty(value = "赠予人姓名")
    private String acceptUserName;

    /** 转赠有效开始日期 */
    @ApiModelProperty(value = "转赠有效开始日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validStartDate;

    /** 转赠有效截止日期 */
    @ApiModelProperty(value = "转赠有效截止日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validEndDate;

    /** 创建时间 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "转赠日期")
    private Date createTm;

    /** 更新时间 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date updateTm;

    //PC端展示属性
    /** 申请人姓名 */
    @ApiModelProperty(value = "申请人姓名")
    private String userName;

    /** 申请人工号 */
    @ApiModelProperty(value = "申请人工号")
    private String userJobNumber;

    /** 赠予人工号 */
    @ApiModelProperty(value = "赠予人工号")
    private String acceptUserJobNumber;
}
