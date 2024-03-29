package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;
import java.util.List;

/**
 * @author
 * @date 2023/9/7
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "车辆审核查询对象（单个/批量）")
public class UserVerifyReq extends PageRequest {

    /** id */
    @ApiModelProperty(value = "id，单个查询时使用此字段")
    private Long id;

    /** 车牌号 */
    @ApiModelProperty(value = "车牌号，小程序端使用")
    private String plateNo;

    /** 申请人 */
    @ApiModelProperty(value = "申请人")
    private String userName;

    /** 状态(0:默认，1:待审核，2:审核不通过,3:审核通过) */
    @ApiModelProperty(value = "状态(0:默认，1:待审核，2:审核不通过,3:审核通过)")
    private Integer state;

    /** 申请日期（起） */
    @ApiModelProperty(value = "申请日期（起）")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date startDate;

    /** 申请日期（止） */
    @ApiModelProperty(value = "申请日期（止）")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date endDate;
}
