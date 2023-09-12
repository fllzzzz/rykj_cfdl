package com.cf.parking.api.response;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 摇号黑名单
 * @author
 * @date 2023/09/05
 */
@Data
@ApiModel(description = "摇号黑名单查询结果")
public class LotteryBlackListRsp {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** userId */
    @ApiModelProperty(value = "userId")
    private Long userId;

    /** 工号 */
    @ApiModelProperty(value = "工号")
    private String jobNumber;

    /** 姓名 */
    @ApiModelProperty(value = "姓名")
    private String name;

    /** 原因 */
    @ApiModelProperty(value = "原因")
    private String reason;

    /** 创建时间 */
    @ApiModelProperty(value = "加入时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss",timezone = "GMT+8")
    private Date createTm;

    /** 更新时间 */
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss",timezone = "GMT+8")
    private Date updateTm;
}
