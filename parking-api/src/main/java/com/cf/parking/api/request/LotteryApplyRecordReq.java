package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号申请记录
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
public class LotteryApplyRecordReq extends PageRequest {

    /** id ，删除时使用*/
    private Long id;

    /** 摇号结果(-1：未开号；0：未中；xx：对应停车场的区域编号) */
    private String result;

    /** 开始期号 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date startDate;

    /** 结束期号 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date endDate;
}
