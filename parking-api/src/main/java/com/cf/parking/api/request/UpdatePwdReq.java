package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;


@Data
@ApiModel(value = "修改密码入参")
public class UpdatePwdReq implements Serializable {

    @ApiModelProperty(value = "原密码")
    private String oldPassword;
    @ApiModelProperty(value = "新密码")
    private String newPassword;

}
