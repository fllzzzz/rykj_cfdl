package com.cf.parking.dao.po;

import java.util.Date;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
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
    @TableId(value = "id", type =  IdType.INPUT )
    private Long id;

    /** 摇号批次id */
    private Long batchId;

    /** 期号 */
    private Date batchNum;

    /** 车位有效开始日期 */
    private Date validStartDate;

    /** 车位有效截止日期 */
    private Date validEndDate;

    /** 申请人id */
    private Long userId;

    /** 申请人姓名*/
    private String userName;

    /** 申请人工号 */
    private String jobNumber;

    /** 申请状态(0：取消申请；1：申请) */
    private String applyState;

    /** 摇号结果(-1：未开号；0：未中；xx：对应停车场的区域编号) */
    private String result;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
    
    /** 停车场 */
    private String parkingLotCode;


}
