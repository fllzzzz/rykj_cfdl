package com.cf.parking.facade.bo;

import lombok.Data;

/**
 * @author
 * @date 2023/9/26
 */
@Data
public class ParkingLotImageInfoBO {

    /** 图片名称 */
    private String name;

    /** 图片信息（base64字符串） */
    private String url;
}
