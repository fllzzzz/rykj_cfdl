package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 停车场新增/修改园区的入口对象
 * @author
 * @date 2023/9/13
 */
@Data
@Accessors(chain = true)
public class ParkingLotAreaEntranceOptReq {
    //入口名称
    @ApiModelProperty(value = "入口名称")
    private String region;

    //入口编号
    @ApiModelProperty(value = "入口编号")
    private String regionCode;
}
