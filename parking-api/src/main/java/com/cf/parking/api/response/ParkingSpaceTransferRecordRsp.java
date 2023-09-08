package com.cf.parking.api.response;

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

    /** 申请人userId */
    @ApiModelProperty(value = "申请人userId")
    private Long userId;

    /** 赠予人姓名 */
    @ApiModelProperty(value = "赠予人姓名")
    private String acceptUserName;

    /** 转赠有效开始日期 */
    @ApiModelProperty(value = "转赠有效开始日期")
    private Date validStartDate;

    /** 转赠有效截止日期 */
    @ApiModelProperty(value = "转赠有效截止日期")
    private Date validEndDate;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
