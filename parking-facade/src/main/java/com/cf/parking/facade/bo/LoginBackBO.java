package com.cf.parking.facade.bo;

import io.swagger.annotations.ApiModel;
import lombok.Data;

import java.io.Serializable;

@Data
@ApiModel("登陆返回信息")
public class LoginBackBO implements Serializable {
    private static final long serialVersionUID = -8965779987437724291L;

    private String token;

    private String adminName;
    private Integer isSuper;

}
