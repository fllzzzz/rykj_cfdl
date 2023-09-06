package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号批次
 * @author
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class LotteryBatchDTO extends PageRequest {

    /** 开始期号 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date startDate;

    /** 结束期号 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date endDate;

    /** 摇号规则轮数 */
    private Long roundId;

    /** 状态（0：待通知；1：已通知；2：已结束） */
    private String state;
}
