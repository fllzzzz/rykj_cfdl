package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author lpy
 * @date 2023-03-28
 * @description 过车记录
 */
@Data
@Accessors(chain = true)
public class CrossRecordsRsp {


    @ApiModelProperty(value = "过车记录id")
    private Long crossRecordsId;

    @ApiModelProperty(value = "停车库名称")
    private String parkingTitle;

    @ApiModelProperty(value = "车牌号")
    private String plateNo;

    @ApiModelProperty(value = "是否出场")
    private String vehicleOut;

    @ApiModelProperty(value = "过车记录唯一标识")
    private String crossRecordSyscode;

    @ApiModelProperty(value = "停车库唯一标识")
    private String parkSyscode;

    @ApiModelProperty(value = "出入口唯一标识")
    private String entranceSyscode;

    @ApiModelProperty(value = "出入口名称")
    private String entranceName;

    @ApiModelProperty(value = "通行时间")
    private String crossTime;

}
