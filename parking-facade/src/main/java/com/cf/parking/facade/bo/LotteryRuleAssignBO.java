package com.cf.parking.facade.bo;

import lombok.Data;

import java.util.Date;

/**
 * 摇号规则-停车场分配
 * @author
 * @date 2023/9/5
 */
@Data
public class LotteryRuleAssignBO {
    /** id */
    private Long id;

    /** 分配类型（按部门分配、按人员分配） */
    private String type;

    /** 分配类型编码（section：部门；person：人员） */
    private String code;

    /** 名称（部门或者人员名称） */
    private String name;

    /** 停车场，多个间逗号间隔 */
    private String parkingLotName;

    /** 状态(0：停用，1：启用) */
    private String state;

    /** 备注 */
    private String remark;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
