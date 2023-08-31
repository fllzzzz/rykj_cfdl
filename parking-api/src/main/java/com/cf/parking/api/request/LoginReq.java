package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;


@Data
@ApiModel(value = "登录入参")
public class LoginReq implements Serializable {
    private static final long serialVersionUID = 2683244820076120841L;

    @ApiModelProperty(value = "工号")
    private String emplNo;

    @ApiModelProperty(value = "密码")
    private String password;

}
