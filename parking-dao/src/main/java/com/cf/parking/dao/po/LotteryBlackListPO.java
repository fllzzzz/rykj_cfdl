package com.cf.parking.dao.po;

import java.util.Date;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 摇号黑名单对象 lottery_black_list
 * 
 * @author
 * @date 2023-09-05
 */
@Data
@TableName("lottery_black_list")
@Accessors(chain = true)
public class LotteryBlackListPO
{
    /** id */
    @TableId(value = "id", type =  IdType.INPUT )
    private Long id;

    /** userId */
    private Long userId;

    /** 工号 */
    private String jobNumber;

    /** 姓名 */
    private String name;

    /** 原因 */
    private String reason;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

}
