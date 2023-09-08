package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号规则-停车场分配
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "摇号规则-停车场分配查询对象（单个/批量）")
public class LotteryRuleAssignReq {

    /** id */
    @ApiModelProperty(value = "id，单个查询或删除时使用此字段")
    private Long id;

    /** 分配类型（1：按部门分配；2：按人员分配） */
    @ApiModelProperty(value = "分配类型（1：按部门分配；2：按人员分配）")
    private String type;

    /** 部门编码/人员工号 */
    @ApiModelProperty(value = "部门编码/人员工号")
    private String code;

    /** 名称（部门或者人员名称） */
    @ApiModelProperty(value = "名称（部门或者人员名称）")
    private String name;

    /** 停车场(编号)，多个间逗号间隔 */
    @ApiModelProperty(value = "停车场(编号)")
    private String parkingLotCode;

    /** 状态(0：停用，1：启用) */
    @ApiModelProperty(value = "状态(0：停用，1：启用)")
    private String state;

}
