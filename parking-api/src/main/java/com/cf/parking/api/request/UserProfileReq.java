package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Data
public class UserProfileReq {
    @ApiModelProperty(value = "userId")
    private Long userId;

    @ApiModelProperty(value = "手机号")
    private String mobile;

    /**
     * 居住地
     */
    @ApiModelProperty(value = "居住地")
    private String livePlace;

    /**
     * 车牌号
     */
    @ApiModelProperty(value = "车牌号")
    private String plateNo;
}
