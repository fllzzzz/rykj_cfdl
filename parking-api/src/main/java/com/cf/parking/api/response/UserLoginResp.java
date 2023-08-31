package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;


@Data
public class UserLoginResp implements Serializable {
    private static final long serialVersionUID = -4261336516190224656L;

    @ApiModelProperty("用户id")
    private Long userId;

    @ApiModelProperty("openId")
    private String openId;

    @ApiModelProperty("token")
    private String token;

    @ApiModelProperty("姓名")
    private String name;

    @ApiModelProperty("工号")
    private String jobNumber;

    @ApiModelProperty("手机号")
    private String mobile;

    @ApiModelProperty("头像")
    private String avatar;
}
