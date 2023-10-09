package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号规则描述实体对象
 * @author
 * @date 2023/10/8
 */
@Data
@TableName("lottery_rule_description")
@Accessors(chain = true)
public class LotteryRuleDescriptionPO {

    /** id */
    @TableId(value = "id", type =  IdType.INPUT )
    private Long id;

    /** 规则描述 */
    private String description;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

}
