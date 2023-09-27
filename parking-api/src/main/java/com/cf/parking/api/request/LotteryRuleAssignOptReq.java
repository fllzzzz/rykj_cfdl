package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;
import java.util.List;

/**
 * 摇号规则-停车场分配操作对象
 * @author
 * @date 2023/9/6
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "摇号规则-停车场分配操作对象（新增/修改）")
public class LotteryRuleAssignOptReq {

    /** id */
    private Long id;

    /** 摇号规则-分配轮次id */
    @ApiModelProperty(value = "摇号规则-分配轮次id")
    private Long roundId;

    /** 分配类型（1：按部门分配；2：按人员分配） */
    @ApiModelProperty(value = "分配类型（1：按部门分配；2：按人员分配）")
    private String type;

    /** 部门编码/人员工号列表 */
    @ApiModelProperty(value = "部门编码/人员工号列表")
    private List<String> codeArr;


    /** 状态(0：停用，1：启用) */
    @ApiModelProperty(value = "状态(0：停用，1：启用)")
    private String state;

    /** 备注 */
    @ApiModelProperty(value = "备注")
    private String remark;

}
