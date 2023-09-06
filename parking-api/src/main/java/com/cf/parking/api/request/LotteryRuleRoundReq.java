package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号规则-轮数
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "摇号规则-轮数查询对象（单个/批量）")
public class LotteryRuleRoundReq {

    /** id */
    @ApiModelProperty(value = "id，单个查询或删除时使用此字段")
    private Long id;

    /** 轮数名称 */
    @ApiModelProperty(value = "轮数名称")
    private String name;

    /** 状态(0：停用，1：启用) */
    @ApiModelProperty(value = "状态(0：停用，1：启用) ")
    private String state;

}
