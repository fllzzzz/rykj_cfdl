package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import lombok.Data;

/**
 * @author
 * @date 2023/9/26
 */
@Data
@ApiModel(description = "停车场图片信息")
public class ParkingLotImageInfoRsp {

    /** 图片名称 */
    private String name;

    /** 图片信息（base64字符串） */
    private String url;
}
