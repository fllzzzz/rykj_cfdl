package com.cf.parking.facade.enums;

/**
 * 进出厂枚举类
 */
public enum VehicleStateEnum {
    IN(0, "进场"),
    OUT(1, "出场");

    VehicleStateEnum(Integer code, String msg) {
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
