package com.cf.parking.facade.bo;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;

import java.util.Date;

/**
 * 摇号批次
 * @author
 * @date 2023/9/5
 */
@Data
public class LotteryBatchBO {

    /** id */
    private Long id;

    /** 期号 */
    private Date batchNum;

    /** 车位数量 */
    private Long parkingAmount;

    /** 摇号轮数，多个间逗号间隔 */
    private String roundId;

    /** 摇号轮数数组*/
    private Long[] roundIdArr;

    /** 摇号规则：根据id查询摇号轮数名称，多个间逗号间隔 */
    private String lotteryRule;

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
