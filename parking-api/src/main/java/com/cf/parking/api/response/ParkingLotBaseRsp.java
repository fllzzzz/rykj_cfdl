package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 停车场列表
 * @author
 * @date 2023/9/13
 */
@Data
@ApiModel(description = "停车场列表")
@Accessors(chain = true)
public class ParkingLotBaseRsp {

    //停车场区域
    @ApiModelProperty(value = "停车场区域")
    private String region;

    //停车场区域编码
    @ApiModelProperty(value = "停车场区域编码")
    private String regionCode;

}
