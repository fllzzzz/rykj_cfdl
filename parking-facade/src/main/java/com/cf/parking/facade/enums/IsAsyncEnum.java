package com.cf.parking.facade.enums;

/**
 * 通用是否枚举类
 */
public enum IsAsyncEnum {
    TRUE(1, "是"),
    FALSE(0, "否");

    IsAsyncEnum(Integer code, String msg) {
        this.code = code;
        this.msg = msg;
    }

    private Integer code;
    private String msg;

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    public String getMsg() {
        return msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }
}
