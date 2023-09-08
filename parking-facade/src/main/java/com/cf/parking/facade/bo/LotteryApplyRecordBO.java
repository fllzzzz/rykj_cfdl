package com.cf.parking.facade.bo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号申请记录
 * @author
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class LotteryApplyRecordBO {

    /** id */
    private Long id;

    /** 期号 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date batchNum;

    /** 停车场编号 */
    private String parkingLotCode;

    /** 车位有效开始日期 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validStartDate;

    /** 车位有效结束日期 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validEndDate;

    /** 摇号结果(-1：未开号；0：未中；xx：对应停车场的区域编号) */
    private String result;


}
