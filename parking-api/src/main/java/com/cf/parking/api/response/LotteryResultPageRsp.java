package com.cf.parking.api.response;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 摇号结果
 * @author
 * @date 2023/09/05
 */
@Data
@ApiModel(description = "摇号结果分页查询结果")
public class LotteryResultPageRsp {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** 摇号批次id */
    @ApiModelProperty(value = "摇号批次id")
    private Long batchId;

    /** 期号 */
    @ApiModelProperty(value = "期号")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date batchNum;

    /** 轮号 */
    @ApiModelProperty(value = "轮号")
    private String RoundName;

    /** 状态（0：待摇号；1：待确认；2：确认中；3：待发布；4：待归档；5：已归档） */
    @ApiModelProperty(value = "状态")
    private String state;

    /** 创建时间 */
    @ApiModelProperty(value = "创建时间")
    private Date createTm;

    /** 更新时间 */
    @ApiModelProperty(value = "更新时间")
    private Date updateTm;
}
