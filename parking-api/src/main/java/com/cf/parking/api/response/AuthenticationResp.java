package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;


@Data
public class AuthenticationResp implements Serializable {
    private static final long serialVersionUID = -4261336516190224656L;

    @ApiModelProperty("生成签名的时间戳")
    private Long timeStamp;

    @ApiModelProperty("自定义固定字符串")
    private String nonceStr;

    @ApiModelProperty("签名")
    private String signature;



}
