package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 摇号规则-轮数
 * @author
 * @date 2023/09/05
 */
@Data
@ApiModel(description = "摇号规则-轮数查询结果")
public class LotteryRuleRoundRsp {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** 轮数名称 */
    @ApiModelProperty(value = "轮数名称")
    private String name;

    /** 停车场(编号)，多个间逗号间隔 */
    @ApiModelProperty(value = "停车场")
    private String parkingLotName;

    /** 状态(0：停用，1：启用) */
    @ApiModelProperty(value = "状态")
    private String state;

    /** 备注 */
    @ApiModelProperty(value = "备注")
    private String remark;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

}
