package com.cf.parking.api.response;

import lombok.Data;

import java.util.Date;

/**
 * 摇号批次
 * @author
 * @date 2023/09/05
 */
@Data
public class LotteryBatchPageRsp {
    /** id */
    private Long id;

    /** 期号 */
    private Date batchNum;

    /** 车位数量 */
    private Long parkingAmount;

    /** 摇号轮数，多个间逗号间隔 */
    private String roundId;

    /** 报名开始时间 */
    private Date applyStartTime;

    /** 报名结束时间 */
    private Date applyEndTime;

    /** 车位有效开始日期 */
    private Date validStartDate;

    /** 车位有效截止日期 */
    private Date validEndDate;

    /** 状态（0：待通知；1：已通知；2：已结束） */
    private String state;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

    /** 备注 */
    private String remark;
}
