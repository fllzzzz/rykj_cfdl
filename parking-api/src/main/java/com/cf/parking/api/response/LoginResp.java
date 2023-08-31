package com.cf.parking.api.response;

import lombok.Data;

import java.io.Serializable;


@Data
public class LoginResp implements Serializable {
    private static final long serialVersionUID = -2575142326254609976L;

    private String token;

    private String adminName;

    private Integer isSuper;

}
