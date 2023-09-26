package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author
 * @date 2023/9/25
 */
@Data
@Accessors(chain = true)
public class ParkingLotImagesDTO {

    /** 图片名称 */
    private String name;

    /** 图片信息（base64字符串） */
    private String url;

}
