package com.cf.parking.api.request;

import cn.hutool.core.date.DateTime;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 摇号批次操作对象
 * @author
 * @date 2023/9/6
 */
@Data
@ApiModel(description = "摇号批次操作对象（新增/修改）")
public class LotteryBatchOptReq {

    /** id */
    private Long id;

    /** 期号 */
    @ApiModelProperty(value = "期号")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date batchNum;

    /** 车位数量 */
    @ApiModelProperty(value = "车位数量")
    private Long parkingAmount;

    /** 摇号轮数，多个间逗号间隔 */
    @ApiModelProperty(value = "摇号轮数")
    private Long[] roundIdArr;

    /** 报名开始时间 */
    @ApiModelProperty(value = "报名开始时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private DateTime applyStartTime;

    /** 报名结束时间 */
    @ApiModelProperty(value = "报名结束时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private DateTime applyEndTime;

    /** 车位有效开始日期 */
    @ApiModelProperty(value = "车位有效开始日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validStartDate;

    /** 车位有效截止日期 */
    @ApiModelProperty(value = "车位有效截止日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validEndDate;

    /** 状态（0：待通知；1：已通知；2：已结束） */
    @ApiModelProperty(value = "状态")
    private String state;

    /** 备注 */
    @ApiModelProperty(value = "备注")
    private String remark;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

    //前端传入的时间数组
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private DateTime[] dialogApplyTime;

    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private DateTime[] validityDate;
}
