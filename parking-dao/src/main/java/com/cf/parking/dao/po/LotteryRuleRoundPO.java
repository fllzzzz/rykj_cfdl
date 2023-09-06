package com.cf.parking.dao.po;

import java.util.Date;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 摇号规则-轮数对象 lottery_rule_round
 * 
 * @author
 * @date 2023-09-05
 */
@Data
@TableName("lottery_rule_round")
@Accessors(chain = true)
public class LotteryRuleRoundPO
{
    /** id */
    private Long id;

    /** 轮数名称 */
    private String name;

    /** 停车场(编号)，多个间逗号间隔 */
    private String parkingLotCode;

    /** 状态(0：停用，1：启用) */
    private String state;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

    /** 备注 */
    private String remark;
}
