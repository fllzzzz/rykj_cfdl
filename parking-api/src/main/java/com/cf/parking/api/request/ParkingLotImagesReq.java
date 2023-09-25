package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author
 * @date 2023/9/25
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "停车场图片信息")
public class ParkingLotImagesReq {

    /** 图片名称 */
    @ApiModelProperty(value = "图片名称")
    private String name;

    /** 图片信息（base64字符串） */
    @ApiModelProperty(value = "图片信息（base64字符串）")
    private String url;


}
