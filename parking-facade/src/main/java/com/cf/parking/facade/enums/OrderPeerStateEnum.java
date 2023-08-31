package com.cf.parking.facade.enums;

public enum OrderPeerStateEnum {
    ORDER_PEER_REQUESTED(1, "已请求"),
    ORDER_PEER_CANCEL_REQUEST(2, "取消请求"),
    ORDER_PEER_FAIL(3, "未成单"),
    ORDER_PEER_CONFIRMED(4, "已确认"),
    ORDER_PEER_CANCEL(5, "取消订单"),
    ORDER_PEER_COMPLETE(6, "已结束"),
    ORDER_PEER_CONFIRM_RIDING(7, "乘客确认上车"),
    ORDER_PEER_OUT_TIME(8, "乘客超时");

    OrderPeerStateEnum(Integer code, String msg) {
        this.code = code;
        this.msg = msg;
    }

    private Integer code;
    private String msg;

    public Integer getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public String getMsg() {
        return msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }
}
