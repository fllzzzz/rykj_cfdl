package com.cf.parking.services.enums;

/**
 * 摇号申请状态
 * @author
 * @date 2023/9/19
 */
public enum LotteryApplyRecordStateEnum {

    //申请状态
    CANCEL("0","取消申请"),
    HAVE_APPLIED("1","已申请"),


    //摇号结果
    NOTOPEN("-1","未开号"),
    NOTGET("0","未中"),
    GET("1","摇中");

    private String state;

    private String remark;

    private LotteryApplyRecordStateEnum(String state, String remark) {
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
