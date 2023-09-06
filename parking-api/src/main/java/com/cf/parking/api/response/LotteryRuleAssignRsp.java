package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 摇号规则-停车场分配
 * @author
 * @date 2023/09/05
 */
@Data
@ApiModel(description = "摇号规则-停车场分配查询结果")
public class LotteryRuleAssignRsp {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** 分配类型（按部门分配、按人员分配） */
    @ApiModelProperty(value = "分配类型")
    private String type;

    /** 分配类型编码（section：部门；person：人员） */
    @ApiModelProperty(value = "分配类型编码")
    private String code;

    /** 名称（部门或者人员名称） */
    @ApiModelProperty(value = "名称")
    private String name;

    /** 停车场，多个间逗号间隔 */
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