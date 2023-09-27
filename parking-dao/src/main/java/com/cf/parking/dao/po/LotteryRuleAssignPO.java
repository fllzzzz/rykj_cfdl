package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号规则-停车场分配对象 lottery_rule_assign
 * 
 * @author
 * @date 2023-09-05
 */
@Data
@TableName("lottery_rule_assign")
@Accessors(chain = true)
public class LotteryRuleAssignPO
{
    /** id */
    @TableId(value = "id", type =  IdType.INPUT )
    private Long id;

    /** 分配类型（1：按部门分配；2：按人员分配） */
    private String type;

    /** 部门编码/人员工号 */
    private String code;

    /** 名称（部门或者人员名称） */
    private String name;

    /** 停车场编号 */
    private String parkingLotCode;

    /** 停车场名称 */
    private String parkingLotRegion;

    /** 状态(0：停用，1：启用) */
    private String state;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

    /** 备注 */
    private String remark;
    
    /**
     * 轮次id
     */
    private Long roundId;

    /**
     * 轮次名称
     */
    private String roundName;
}
