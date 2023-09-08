package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号结果详情对象 lottery_result_detail
 * 
 * @author
 * @date 2023-09-05
 */
@Data
@TableName("lottery_result_detail")
@Accessors(chain = true)
public class LotteryResultDetailPO
{
    /** id */
    private Long id;

    /** 摇号结果表id */
    private Long resultId;

    /** 停车场编号 */
    private String parkingLotCode;

    /** 用户id */
    private Long userId;

    /** 用户姓名 */
    private String userName;

    /** 用户工号 */
    private String userJobNumber;

    /** 状态（0：未同步；1：同步成功；2：同步失败） */
    private String state;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

}
