package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * @author
 * @date 2023/9/6
 */
@Data
@ApiModel(description = "摇号黑名单操作对象（新增/修改）")
public class LotteryBlackListOptReq {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** 工号 */
    @ApiModelProperty(value = "工号")
    private String code;

    /** 姓名 */
    @ApiModelProperty(value = "姓名")
    private String name;

    /** 原因 */
    @ApiModelProperty(value = "原因")
    private String reason;
    
    private Integer type;

}
