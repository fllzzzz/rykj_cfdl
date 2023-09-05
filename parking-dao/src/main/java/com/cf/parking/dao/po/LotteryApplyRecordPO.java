package com.cf.parking.dao.po;

import java.util.Date;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 摇号申请记录对象 lottery_apply_record
 * 
 * @author
 * @date 2023-09-05
 */
@Data
@TableName("lottery_apply_record")
@Accessors(chain = true)
public class LotteryApplyRecordPO
{
    /** id */
    private Long id;

    /** 摇号批次id */
    private Long batchId;

    /** 用户 */
    private Long userId;

    /** 车牌号 */
    private String plateNo;

    /** 申请状态(0：取消申请；1：申请) */
    private String applyState;

    /** 摇号结果(-1：未开号；0：未中；xx：对应停车场的区域编号) */
    private String result;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;


}
