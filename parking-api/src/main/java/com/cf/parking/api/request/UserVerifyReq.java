package com.cf.parking.api.request;

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
public class UserVerifyReq {

    /** id */
    @ApiModelProperty(value = "id，单个查询时使用此字段")
    private Long id;

    /** 申请人 */
    @ApiModelProperty(value = "申请人")
    private String userName;

    /** 状态(0:待审核，1:审核失败,2:审核通过 3:审核不通过) */
    @ApiModelProperty(value = "状态(1:待审核，2:审核失败,3:审核成功)")
    private String state;

    /** 申请日期（起） */
    @ApiModelProperty(value = "申请日期（起）")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date startDate;

    /** 申请日期（止） */
    @ApiModelProperty(value = "申请日期（止）")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date endDate;
}
