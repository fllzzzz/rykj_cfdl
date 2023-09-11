package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author
 * @date 2023/9/11
 */
@Data
@ApiModel(description = "摇号规则-轮数基础查询结果")
public class LotteryRuleRoundBaseRsp {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** 轮数名称 */
    @ApiModelProperty(value = "轮数名称")
    private String name;
}
