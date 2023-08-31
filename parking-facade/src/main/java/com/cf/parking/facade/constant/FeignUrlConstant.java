package com.cf.parking.facade.constant;

/**
 * @author: lpy
 * @Date: 2023/03/27
 */
public interface FeignUrlConstant {
    /**
     * 黑名单批量删除
     */
    String BLACK_LIST_DELETION_URL = "artemis/api/pms/v1/alarmCar/deletion";

    /**
     * 查询车辆包期
     */
    String CAR_CHARGE_URL = "artemis/api/pms/v1/car/charge/page";

    /**
     * 黑名单添加
     */
    String BLACK_LIST_ADD_URL = "artemis/api/pms/v1/alarmCar/addition";
}
