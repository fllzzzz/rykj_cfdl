package com.cf.parking.dao.po;

import java.util.Date;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 摇号结果对象 lottery_result
 * 
 * @author
 * @date 2023-09-05
 */
@Data
@TableName("lottery_result")
@Accessors(chain = true)
public class LotteryResultPO
{
    /** id */
    private Long id;

    /** 摇号批次id */
    private Long batchId;
    
    /**
     * 轮次id
     */
    private Long roundId;

    /** 状态（0：待摇号；1：待确认；2：确认中；3：待发布；4：待归档） */
    private String state;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;


}
