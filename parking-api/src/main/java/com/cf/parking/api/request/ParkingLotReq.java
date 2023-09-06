package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 停车场对象
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "停车场查询对象（单个/批量）")
public class ParkingLotReq {

    /** id */
    @ApiModelProperty(value = "id，单个查询或删除时使用此字段")
    private Long id;

    /** 区域 */
    @ApiModelProperty(value = "区域")
    private String region;

    /** 区域编号 */
    @ApiModelProperty(value = "区域编号")
    private String regionCode;

    /** 类型(0：不可摇号，1：可摇号) */
    @ApiModelProperty(value = "类型(0：不可摇号，1：可摇号)")
    private String type;

}
