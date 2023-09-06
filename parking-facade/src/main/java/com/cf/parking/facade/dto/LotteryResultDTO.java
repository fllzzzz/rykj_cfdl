package com.cf.parking.facade.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号结果
 * @author
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class LotteryResultDTO {
    /** id */
    private Long id;

    /** 开始期号 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date startDate;

    /** 结束期号 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date endDate;

    /** 状态（0：待摇号；1：待确认；2：确认中；3：待发布；4：待归档） */
    private String state;

    /** 摇号规则 */
    private Long roundId;
}
