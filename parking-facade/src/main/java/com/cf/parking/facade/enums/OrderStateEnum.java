package com.cf.parking.facade.enums;

/**
 * @author lpy
 * @Date
 */

public enum OrderStateEnum {
    ORDER_NOT_START(1, "未开始"),
    ORDER_ING(2, "进行中"),
    ORDER_CANCEL(3, "已取消"),
    ORDER_COMPLETE(4, "已完成"),
    ORDER_OUT_TIME(5, "超时");

    OrderStateEnum(Integer code, String msg) {
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
