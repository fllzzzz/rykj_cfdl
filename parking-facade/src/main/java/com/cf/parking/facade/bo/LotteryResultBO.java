package com.cf.parking.facade.bo;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;

import java.util.Date;

/**
 * 摇号结果
 * @author
 * @date 2023/9/5
 */
@Data
public class LotteryResultBO {
    /** id */
    private Long id;

    /** 摇号批次id */
    private Long batchId;

    /** 期号 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date batchNum;

    /** 轮号 */
    private Date RoundName;

    /** 状态（0：待摇号；1：待确认；2：确认中；3：待发布；4：待归档） */
    private String state;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
