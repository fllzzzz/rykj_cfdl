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
    @JsonFormat(pattern = "yyyy-MM-dd",timezone = "GMT+8")
    private Date validStartDate;

    /** 转赠日期（止） */
    @ApiModelProperty(value = "转赠日期（止）")
    @JsonFormat(pattern = "yyyy-MM-dd",timezone = "GMT+8")
    private Date validEndDate;

    /** userId */
    private Long userId;

}
