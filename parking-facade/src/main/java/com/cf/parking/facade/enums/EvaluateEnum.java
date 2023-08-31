package com.cf.parking.facade.enums;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
public enum EvaluateEnum {
    EVALUATED(1, "已评价"),
    NOT_EVALUATED(2, "未评价");
    private Integer code;
    private String name;

    EvaluateEnum(Integer code, String name) {
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
