package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号规则-轮数操作对象
 * @author
 * @date 2023/9/6
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "摇号规则-轮数操作对象（新增/修改）")
public class LotteryRuleRoundOptReq extends PageRequest {

    /** id */
    private Long id;

    /** 轮数名称 */
    @ApiModelProperty(value = "轮数名称")
    private String name;

    /** 停车场(编号)，多个间逗号间隔 */
    @ApiModelProperty(value = "停车场(编号)，多个间逗号间隔")
    private String parkingLotCode;

    /** 状态(0：停用，1：启用) */
    @ApiModelProperty(value = "状态(0：停用，1：启用)")
    private String state;

    /** 备注 */
    @ApiModelProperty(value = "备注")
    private String remark;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

}
