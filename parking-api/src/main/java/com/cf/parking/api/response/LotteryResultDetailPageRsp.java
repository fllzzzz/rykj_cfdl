package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 摇号结果详情
 * @author
 * @date 2023/09/05
 */
@Data
@ApiModel(description = "摇号结果详情查询结果")
public class LotteryResultDetailPageRsp {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** 摇号结果表id */
    @ApiModelProperty(value = "摇号结果表id")
    private Long resultId;

    /** 停车场名称 */
    @ApiModelProperty(value = "停车场名称")
    private String parkingLotName;

    /** 停车场编号 */
    @ApiModelProperty(value = "停车场编号")
    private String parkingLotCode;

    /** 用户 */
    @ApiModelProperty(value = "用户id")
    private Long userId;

    /** 用户姓名 */
    @ApiModelProperty(value = "用户姓名")
    private String userName;

    /** 用户工号 */
    @ApiModelProperty(value = "用户工号")
    private String userJobNumber;

    /** 状态（0：未同步；1：同步成功；2：同步失败） */
    @ApiModelProperty(value = "状态")
    private String state;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
