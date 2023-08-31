package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

@Data
@Accessors(chain = true)
public class UserLoginBO implements Serializable {

    private Long userId;

    private String openId;

    private String token;
    /**
     * 姓名
     */
    private String name;

    /**
     * 工号
     */
    private String jobNumber;

    /**
     * 手机号
     */
    private String mobile;

    /**
     * 头像
     */
    private String avatar;
}
