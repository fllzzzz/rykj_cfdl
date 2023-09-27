package com.cf.parking.facade.bo;

import lombok.Data;

import java.util.Date;
import java.util.List;

/**
 * 摇号规则-停车场分配
 * @author
 * @date 2023/9/5
 */
@Data
public class LotteryRuleAssignBO {
    /** id */
    private Long id;

    /** 摇号规则-分配轮次id */
    private Long roundId;

    /** 摇号规则-分配轮次id */
    private String roundName;

    /** 分配类型（1：按部门分配；2：按人员分配） */
    private String type;

    /** 部门编码/人员工号 */
    private String code;

    private List<String> codeArr;

    /** 名称（部门或者人员名称） */
    private String name;

    /** 停车场编号 */
    private String parkingLotCode;

    /** 停车场 */
    private String parkingLotRegion;

    /** 状态(0：停用，1：启用) */
    private String state;

    /** 备注 */
    private String remark;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
