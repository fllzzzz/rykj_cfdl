package com.cf.parking.facade.enums;
/**
 * @author lpy
 * @date 2022/10/19
 */

public enum NoticedEnum {
    NOT_NOTICED(1, "未通知"),
    NOTICED(2, "已通知");
    private Integer code;
    private String name;

    NoticedEnum(Integer code, String name) {
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
