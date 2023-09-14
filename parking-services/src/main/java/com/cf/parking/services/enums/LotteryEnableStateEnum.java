package com.cf.parking.services.enums;

/**
 * 是否可摇号
 * @author
 * @date 2023/9/14
 */
public enum LotteryEnableStateEnum {
    ENABLE("0","可摇号"),
    DISABLE("1","不可摇号");

    private String state;

    private String remark;

    private LotteryEnableStateEnum(String state, String remark) {
        this.state = state;
        this.remark = remark;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public String getRemark() {
        return remark;
    }

    public void setRemark(String remark) {
        this.remark = remark;
    }
}
