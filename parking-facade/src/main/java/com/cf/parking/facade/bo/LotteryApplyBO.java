package com.cf.parking.facade.bo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号申请页面信息
 * @author
 * @date 2023/9/18
 */
@Data
@Accessors(chain = true)
public class LotteryApplyBO {

    /** 批次id */
    private Long batchId;

    /** 报名开始时间 */
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss",timezone = "GMT+8")
    private Date applyStartTime;

    /** 报名结束时间 */
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss",timezone = "GMT+8")
    private Date applyEndTime;

    /** 车位有效开始日期 */
    @JsonFormat(pattern = "yyyy-MM-dd",timezone = "GMT+8")
    private Date validStartDate;

    /** 车位有效结束日期 */
    @JsonFormat(pattern = "yyyy-MM-dd",timezone = "GMT+8")
    private Date validEndDate;

    /** 摇号结果 */
    private String result;

    /** 摇号结果颜色 */
    private Integer resultColor;

    /** 报名时间状态，true：报名时间内；false：报名时间外 */
    private Boolean timeState;

    /** 是否已申请状态，true：已申请；false：未申请 */
    private Boolean applyState;
}
