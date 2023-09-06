package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号批次
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "摇号批次查询对象（单个/批量）")
public class LotteryBatchReq extends PageRequest {

    /** id */
    @ApiModelProperty(value = "id，单个查询或删除时使用此字段")
    private Long id;

    /** 开始期号 */
    @ApiModelProperty(value = "开始期号")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date startDate;

    /** 结束期号 */
    @ApiModelProperty(value = "结束期号")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date endDate;

    /** 摇号规则轮数 */
    @ApiModelProperty(value = "摇号规则轮数")
    private Long roundId;

    /** 状态（0：待通知；1：已通知；2：已结束） */
    @ApiModelProperty(value = "状态")
    private String state;

}
