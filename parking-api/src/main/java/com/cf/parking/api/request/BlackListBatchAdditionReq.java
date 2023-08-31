package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author lpy
 * @date 2023-03-27 09:43:43
 * @description 黑名单记录表
 */
@Data
@Accessors(chain = true)
public class BlackListBatchAdditionReq {

    @ApiModelProperty(value = "工号")
    private String jobNumber;

    @ApiModelProperty(value = "原因")
    private String joinReason;

    @ApiModelProperty(value = "车牌号")
    private String plateNo;

    @ApiModelProperty(value = "姓名")
    private String name;

    @ApiModelProperty(value = "id")
    private Long blackListId;

}
