package com.cf.parking.facade.enums;


import com.cf.support.result.ResultCode;

/**
 * 业务错误信息码表(101开始) 101 102 前三位模块  后面3位序号
 *
 * @author tasher
 * @created 2021/7/20
 */
public enum BizResultCodeEnum implements ResultCode {

    /**
     * 101 基础/其他类
     **/
    OTHER_SYSTEM_ERROR(101001, "系统开小差了，请稍后再试"),

    PARAM_NULL(101002, "参数为空"),

    PARAM_ERROR(101003, "参数有误"),

    LOCK_ERROR(101004, "系统繁忙，请稍后再试"),

    TOKEN_BUILD_ERROR(101005, "token生成失败"),


    /* 102 用户类 */
    USER_UN_EXSIT(102001, "用户不存在"),
    USER_UN_LOGIN(102002, "用户未登录"),
    DING_CODE_USER_ERROR(102003, "code获取user失败"),
    USER_CLOSED(102004, "用户已封禁"),
    CANNOT_EVALUATE(102005, "当前状态无法评价"),
    ALREADY_EVALUATED(102006, "您当前已评价"),

    USER_PROFILE_UPDATE_ERROR(102007, "用户详情更新失败"),
    EVALUATE_DONE(102008, "评价已完成，请刷新页面"),
    EVALUATE_USER_ERROR(102009, "当前登录用户无评价权限"),
    SCORE_OPT_FAIL(102010, "积分操作失败，请重试"),
    SCORE_OPT_DONE(102011, "积分操作已完成，请刷新页面"),
    USER_ERROR(102012, "用户无权限"),
    COMMON_ADDRESS_LIMIT(102013, "常用地址数已达上限(5)"),

    /* 103 订单类*/
    MORE_THAN_TWO_ORDER(103001, "发单次数已到达上限，每天0-12点、12-24点只能各发一单。"),
    CANNOT_SETOUT_BEFORE_NOW(103002, "出发时间不能早于当前时间"),
    STATE_CHANGED_PLEASE_REFRESH(103003, "状态已更新，请刷新页面"),
    PERMISSION_DENIED(103004, "您当前无权限操作"),
    CANNOT_CANCEL_IN_CURRENT_STATE(103005, "当前状态不能取消"),
    NO_ORDER(103006, "没有当前订单"),
    ORDER_BE_OVERDUE(103007, "该订单已过期，请刷新"),
    ORDER_NOT_SAME(103008, "司机不能请求自己的订单"),
    ORDER_STATE_ERROR(103009, "当前订单状态错误,请刷新"),
    EVALUATE_ERR(103010, "评价失败"),
    ORDER_REQUEST_CONFIRM(103011, "存在进行中或已完成的订单，无法请求搭车"),
    ORDER_REQUEST_LIMIT(103012, "请求搭车次数已达上限"),
    PLATE_NO_IS_NULL(103013, "请去个人资料填写车牌号"),
    ORDER_OPT_FAIL(103014, "当前状态下，订单不能进行操作"),
    ORDER_TIME_BEFORE_NOW(103015, "已错过出发时间"),
    MAXIMUM_REQUESTS(103016, "该订单请求次数已达上限"),
    NO_PEER_PASSENGER(103017, "没有同行人"),
    ORDER_PEER_NULL(103018, "同行记录不存在"),
    ORDER_PEER_ORDER_STATE_MATCH(103019, "订单状态同行记录状态不匹配"),
    ORDER_ALREADY_REQUEST(103020, "该订单已请求，无法再次请求"),
    ORDER_PASSENGER_HAS_BEEN_CONFIRMED_BY_OTHER_DRIVER(103021, "该乘客已经被其他司机确认同行"),
    FAILED_TO_GET_GEOGRAPHIC_LOCATION(103022, "获取位置失败，请确认手机定位权限是否开启"),
    DEPARTURE_TIME_NOT_ARRIVE(103023, "出发时间未到，是否确认上车"),
    ;

    BizResultCodeEnum(Integer resultCode, String resultMsg) {
        this.code = resultCode;
        this.msg = resultMsg;
    }

    private Integer code;
    private String msg;

    @Override
    public int getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    @Override
    public String getMsg() {
        return msg;
    }

    @Override
    public void setMsg(String msg) {
        this.msg = msg;
    }

    public String formatMsg(Object... args) {
        return String.format(msg, args);
    }
}
