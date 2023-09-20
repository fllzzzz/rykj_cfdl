package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号结果
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "摇号结果查询对象")
public class LotteryResultReq extends PageRequest {

    /** id */
    @ApiModelProperty(value = "摇号、确认、发布、归档时需要使用此id")
    private Long id;

    /** 开始期号 */
    @ApiModelProperty(value = "开始期号")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date startDate;

    /** 结束期号 */
    @ApiModelProperty(value = "结束期号")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date endDate;

    /** 状态（0：待摇号；1：待确认；2：确认中；3：待发布；4：待归档；5：已归档） */
    @ApiModelProperty(value = "状态")
    private String state;

    /** 摇号规则 */
    @ApiModelProperty(value = "摇号规则")
    private Long roundId;
}
