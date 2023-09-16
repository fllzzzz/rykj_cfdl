package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * @author
 * @date 2023/9/15
 */
@Data
@ApiModel(description = "停车场树形组件")
public class ParkingLotTreeRsp {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** 停车场区域 */
    @ApiModelProperty(value = "停车场区域")
    private String region;

    /** 停车场区域编号 */
    @ApiModelProperty(value = "停车场区域编号")
    private String regionCode;

    /** children */
    @ApiModelProperty(value = "子记录")
    private List<ParkingLotTreeRsp> children;
}
