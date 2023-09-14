package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author
 * @date 2023/9/14
 */
@Data
@ApiModel(description = "停车场园区入口")
public class ParkingLotAreaEntranceRsp {
    //入口名称
    @ApiModelProperty(value = "入口名称")
    private String region;

    //入口编号
    @ApiModelProperty(value = "入口编号")
    private String regionCode;
}
