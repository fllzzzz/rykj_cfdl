package com.cf.parking.facade.enums;
/**
 * @author lpy
 * @date 2022/10/19
 */

public enum OptTypeEnum {
    DRIVER_COMMIT_ORDER(1, "司机提交订单(未开始)"),
    CONFIRM_PEER(2, "司机确认同行(进行中)"),
    PASSENGER_ARRIVE(3, "乘客到达目的地(已完成)"),
    CANCEL_ON_EXPIRE(4, "过期自动取消"),
    DRIVER_CANCEL(5, "司机取消订单"),
    PASSENGER_CANCEL(6, "确认同行后被乘客取消"),
    AUTO_ARRIVE(7, "自动到达目的地(已完成)"),
    PASSENGER_CONFIRM_RIDING(8, "乘客确认上车"),
    OUT_TIME(9, "超时取消订单"),
    ;
    private Integer code;
    private String name;

    OptTypeEnum(Integer code, String name) {
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
