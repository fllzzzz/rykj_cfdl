package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author lpy
 */

@Data
public class OrderDetailPassengerPeerRsp {
    @ApiModelProperty(value = "同行记录id")
    private Long orderPeerId;

    @ApiModelProperty(value = "userId")
    private Long userId;

    @ApiModelProperty(value = "工号")
    private String jobNumber;

    @ApiModelProperty(value = "姓名")
    private String name;

    @ApiModelProperty(value = "头像url")
    private String avatar;

    @ApiModelProperty(value = "手机号")
    private String mobile;

    @ApiModelProperty(value = "title:乘客、车主")
    private String title;
}
