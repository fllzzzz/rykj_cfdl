package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;
import java.util.List;

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

    /** 摇号规则-分配轮次id */
    @ApiModelProperty(value = "摇号规则-分配轮次id")
    private Long roundId;

    /** 分配类型（1：按部门分配；2：按人员分配） */
    @ApiModelProperty(value = "分配类型（1：按部门分配；2：按人员分配）")
    private String type;

    /** 部门编码/人员工号 */
    @ApiModelProperty(value = "部门编码/人员工号")
    private String code;

    /** 名称（部门或者人员名称） */
    @ApiModelProperty(value = "名称（部门或者人员名称）")
    private String name;

    /** 停车场，多个间逗号间隔 */
    @ApiModelProperty(value = "停车场名称")
    private String parkingLotName;

    /** 停车场编号，多个间逗号间隔 */
    @ApiModelProperty(value = "停车场编号")
    private String parkingLotCode;

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
