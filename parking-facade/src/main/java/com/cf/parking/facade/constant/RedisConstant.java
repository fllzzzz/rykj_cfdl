package com.cf.parking.facade.constant;

/**
 * @author weihui
 * @date 2020/10/16
 */
public interface RedisConstant {

    //用户登录锁
    String USER_LOGIN_LOCK_KEY = "visitor:user:login:lock:";
    int USER_LOGIN_LOCK_KEY_WAIT = 0;
    int USER_LOGIN_LOCK_KEY_EXPIRE = 5;


    //定时任务-更新停车状态锁
    String JOB_SYCNDATA_LOCK_KEY = "visitor:job:sycnData:lock";
    int JOB_SYCNDATA_LOCK_KEY_WAIT = 0;
    int JOB_SYCNDATA_LOCK_KEY_EXPIRE = 5;

    //定时任务-每日空闲表统计锁
    String JOB_USER_SPACE_LOCK_KEY = "parking:job:userSpace:lock";
    int JOB_USER_SPACE_LOCK_KEY_WAIT = 0;
    int JOB_USER_SPACE_LOCK_KEY_EXPIRE = 5;

    //订单状态锁
    String ORDER_JOB_SYCN_DATA_LOCK_KEY = "order:job:sycnData:lock";
    //订单
    String ORDER_JOB_SYCN_NO_SURE_KEY = "order:job:noSure:lock";
    //订单状态锁
    String ORDER_JOB_AUTOMATIC_ORDER_LOCK_KEY = "order:job:automaticOrder:lock";
    //订单状态锁
    String ORDER_JOB_ARRIVE_ORDER_LOCK_KEY = "order:job:arriveOrder:lock";
    String ORDER_JOB_COUNT_LOCK_KEY = "order:job:count:lock";

    String PARKING_APP_VERSION = "parking_app_version";

    String PARKING_JOB_ZOMBIE_LOCK_KEY = "parking:job:zombie:lock";
    // 排班数据锁
    String PARKING_JOB_SCHEDULE_LOCK_KEY = "parking:job:schedule:lock";
    // 过车记录锁
    String PARKING_JOB_CROSS_RECORDS_LOCK_KEY = "parking:job:crossRecords:lock";
    String PARKING_JOB_NOT_PARK_LOCK_KEY = "parking:job:notPark:lock";
    
    /**
     * 过期车位
     */
    String PARKING_EXPIRED_LOCK_KEY = "parking:expired:lock";
    
    
    /**
     * 车位生效当天进行闸机录入
     */
    String PARKING_DOWN_LOCK_KEY = "parking:down:lock";
    
    
    /**
     * 把车位信息写到闸机
     */
    String PARKING_SYNC_LOCK_KEY = "parking:sync:lock";
    
    
    /**
     * 结果下发到闸机完成时更新结果表状态
     */
    String PARKING_RESULT_LOCK_KEY = "parking:result:lock";
    
	/**
	 * 请求闸机的token
	 */
	String PARKING_TOKEN = "parking:token";
    
	/**
	 * 定时创建批次任务
	 */
	String AUTO_CREATE_BATCH = "parking:create:batch";
}

