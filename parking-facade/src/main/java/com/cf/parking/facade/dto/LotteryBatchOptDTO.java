package com.cf.parking.facade.dto;

import cn.hutool.core.date.DateTime;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author
 * @date 2023/9/8
 */
@Data
@Accessors(chain = true)
public class LotteryBatchOptDTO {

    /** id */
    private Long id;

    /** 期号 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date batchNum;

    /** 车位数量 */
    private Long parkingAmount;

    /** 摇号轮数，多个间逗号间隔 */
    private Long[] roundIdArr;

    /** 报名开始时间 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private DateTime applyStartTime;

    /** 报名结束时间 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private DateTime applyEndTime;

    /** 车位有效开始日期 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validStartDate;

    /** 车位有效截止日期 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validEndDate;

    /** 状态（0：待通知；1：已通知；2：已结束） */
    private String state;

    /** 备注 */
    @ApiModelProperty(value = "备注")
    private String remark;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
