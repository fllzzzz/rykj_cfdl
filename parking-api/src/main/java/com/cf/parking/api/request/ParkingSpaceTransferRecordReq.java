package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 车位转赠记录
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "停车场查询对象")
public class ParkingSpaceTransferRecordReq extends PageRequest {

    /** 转赠日期（起） */
    @ApiModelProperty(value = "转赠日期（起）")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validStartDate;

    /** 转赠日期（止） */
    @ApiModelProperty(value = "转赠日期（止）")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validEndDate;

    /** userId */
    @ApiModelProperty(value = "用户id，不需要传入")
    private Long userId;

    //PC端查询条件
    /**停车场 */
    @ApiModelProperty(value = "转赠车位")
    private String parkingLotCode;

    /**用户信息（姓名/工号） */
    @ApiModelProperty(value = "用户信息（姓名/工号）")
    private String userInfo;


}
