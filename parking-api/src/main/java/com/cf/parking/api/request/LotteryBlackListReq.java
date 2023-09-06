package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号黑名单
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "摇号黑名单查询对象（单个/批量）")
public class LotteryBlackListReq {

    /** id */
    @ApiModelProperty(value = "id，单个查询或删除时使用此字段")
    private Long id;

    /** 工号 */
    @ApiModelProperty(value = "工号")
    private String jobNumber;

    /** 姓名 */
    @ApiModelProperty(value = "姓名")
    private String name;

}
