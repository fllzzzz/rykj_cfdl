package com.cf.parking.facade.enums;
/**
 * @author lpy
 * @date 2022/10/19
 */

public enum UserTypeEnum {
    PASSENGER(1, "乘客"),
    DRIVER(2, "司机"),
    AUTO(3, "自动"),
    ;
    private Integer code;
    private String name;

    UserTypeEnum(Integer code, String name) {
        this.code = code;
        this.name = name;
    }

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
