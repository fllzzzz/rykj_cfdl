package com.cf.parking.services.enums;

/**
 * 摇号批次状态
 * @author
 * @date 2023/9/18
 */
public enum LotteryBatchStateEnum {
    NEED_NOTIFY("0","待通知"),
    HAVE_NOTIFIED("1","已通知"),
    HAVE_END("2","已结束");


    private String state;

    private String remark;

    private LotteryBatchStateEnum(String state, String remark) {
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
