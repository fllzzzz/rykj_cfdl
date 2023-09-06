package com.cf.parking.api.response;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 摇号申请记录
 * @author
 * @date 2023/09/05
 */
@Data
public class LotteryApplyRecordPageRsp {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** 期号 */
    @ApiModelProperty(value = "期号")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date batchNum;

    /** 车位有效开始日期 */
    @ApiModelProperty(value = "车位有效开始日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validStartDate;

    /** 车位有效结束日期 */
    @ApiModelProperty(value = "车位有效结束日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validEndDate;

    /** 摇号结果(-1：未开号；0：未中；xx：对应停车场的区域编号) */
    @ApiModelProperty(value = "摇号结果")
    private String result;

}
