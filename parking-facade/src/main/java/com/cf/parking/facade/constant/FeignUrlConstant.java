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

    /**
     * 车辆充值
     */
    String CAR_CHARGE_SET_URL = "artemis/api/pms/v1/car/charge";

    /**
     * 取消车辆包期
     */
    String CAR_CHARGE_DEL_URL = "artemis/api/pms/v1/car/charge/deletion";

    /**
     * 获取停车库列表
     */
    String GET_PARK_URL = "artemis/api/resource/v1/park/parkList";

    /**
     * 查询停车库剩余车位数
     */
    String SPACE_NUM_URL = "artemis/api/pms/v1/park/remainSpaceNum";
}
