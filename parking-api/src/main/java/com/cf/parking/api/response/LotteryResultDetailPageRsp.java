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

    /** 轮数 */
    @ApiModelProperty(value = "轮数")
    private Long roundId;

    /** 停车场 */
    @ApiModelProperty(value = "停车场")
    private Long parkingLotName;

    /** 用户 */
    @ApiModelProperty(value = "用户id")
    private Long userId;

    /** 车牌号 */
    @ApiModelProperty(value = "车牌号")
    private String plateNo;

    /** 状态（0：未同步；1：同步成功；2：同步失败） */
    @ApiModelProperty(value = "状态")
    private String state;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
