package com.cf.parking.facade.constant;

import cn.hutool.core.date.DateUtil;

import java.util.Date;

/**
 * @Classname MessageConstant
 * @Date 2022/10/21 12:51
 * @Created by csy
 */
public class MessageConstant {
    public final static String NORM_DATETIME_MINUTE_PATTERN = "yyyy/MM/dd HH:mm";
    private static final String HITCHHIKE_REQUEST_MESSAGE = "%s(%s)请求搭车，可在订单详情页私聊发送定位确认起始点。请点击查看详情按钮查看订单详情,申请时间：%s";
    private static final String HITCHHIKE_REQUEST_PASS_MESSAGE = "%s(%s)已同意您的请求，可在订单详情页私聊发送定位确认起始点，请点击查看详情按钮查看订单详情";

    private static final String ARRIVE_DESTINATION_MESSAGE = "您有订单还未确认完成，请及时在春风春风车小程序处理";
    /**
     * 乘客取消
     */
    private static final String PASSENGER_CANCEL_MESSAGE = "%s(%s)已取消%s的开车订单，请点击查看详情按钮查看订单详情";
    /**
     * 司机取消发送给乘客
     */
    private static final String DRIVER_CANCEL_MESSAGE = "%s(%s)已取消%s的搭车订单，请点击查看详情按钮查看订单详情";
    /**
     * 通知乘客
     */
    private static final String START_PASSENGER_ORDER_MESSAGE = "距离您的%s的搭车订单还有%s分钟开始，请提早准备";

    /**
     * 通知司机
     */
    private static final String START_DRIVER_ORDER_MESSAGE = "距离您的%s的开车订单还有%s分钟开始，请提早准备。";
    /**
     * 通知司机
     */
    public static final String PARKING_TITLE = "春风顺风车通知";

    /**
     * 超时订单通知司机
     */
    public static final String OUT_TIME_ORDER_MESSAGE = "您有订单因对方乘客未确认完成造成超时，请联系顺风车运营同事周锋(CFDL09860)处理";

    /**
     * 拼接乘客要求司机搭车时的信息
     *
     * @param hitchhikeName      乘客姓名
     * @param hitchhikeJobNumber 乘客工号
     * @return 乘客要求司机搭车时的信息
     */
    public static String getHitchhikeRequestMessage(String hitchhikeName
            , String hitchhikeJobNumber) {
        return String.format(HITCHHIKE_REQUEST_MESSAGE, hitchhikeName, hitchhikeJobNumber, DateUtil.format(new Date(), NORM_DATETIME_MINUTE_PATTERN));
    }

    /**
     * 拼接点击确认同行
     *
     * @param driverName         司机姓名
     * @param driverJobNumber    司机工号
     * @return 乘客要求司机搭车时的信息
     */
    public static String getHitchhikePassRequestMessage( String driverName, String driverJobNumber) {
        return String.format(HITCHHIKE_REQUEST_PASS_MESSAGE, driverName, driverJobNumber);
    }

    /**
     * 未点击确认达到目的地的消息
     *
     * @return 消息新
     */
    public static String getArriveDestinationMessage() {
        return ARRIVE_DESTINATION_MESSAGE;
    }

    /**
     * 乘客取消通知司机的信息拼接
     *
     * @param driverName      司机姓名
     * @param driverJobNumber 司机工号
     * @param orderTime       订单时间
     * @return
     */
    public static String getPassengerCancelMessage(String driverName, String driverJobNumber, Date orderTime) {
        return String.format(PASSENGER_CANCEL_MESSAGE, driverName, driverJobNumber
                , DateUtil.format(orderTime, NORM_DATETIME_MINUTE_PATTERN));
    }

    /**
     * 司机取消通知乘客的信息拼接
     *
     * @param hitchhikeName      司机姓名
     * @param hitchhikeJobNumber 司机工号
     * @param orderTime          订单时间
     * @return
     */
    public static String getDriverCancelMessage(String hitchhikeName
            , String hitchhikeJobNumber, Date orderTime) {
        return String.format(DRIVER_CANCEL_MESSAGE, hitchhikeName, hitchhikeJobNumber
                , DateUtil.format(orderTime, NORM_DATETIME_MINUTE_PATTERN));
    }

    /**
     * 开始订单给乘客的提醒
     *
     * @param runMinute 还有多长时间
     * @param orderTime 订单时间
     * @return
     */
    public static String getStartPassengerOrderMessage(Long runMinute
            , Date orderTime) {
        return String.format(START_PASSENGER_ORDER_MESSAGE
                , DateUtil.format(orderTime, NORM_DATETIME_MINUTE_PATTERN), runMinute);
    }

    /**
     * 开始订单给司机的提醒
     *
     * @param runMinute 还有多长时间
     * @param orderTime 订单时间
     * @return
     */
    public static String getStartDriverOrderMessage(Long runMinute
            , Date orderTime) {
        return String.format(START_DRIVER_ORDER_MESSAGE
                , DateUtil.format(orderTime, NORM_DATETIME_MINUTE_PATTERN), runMinute);
    }

}
