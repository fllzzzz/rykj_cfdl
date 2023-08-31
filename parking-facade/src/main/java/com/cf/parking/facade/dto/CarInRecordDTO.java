package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

@Data
@Accessors(chain = true)
public class CarInRecordDTO implements Serializable {
    /**
     * 停车信息唯一标识
     */
    private String inRecordSyscode;

    /**
     * 卡号，和车牌信息不一定同时存在
     */
    private String cardNo;

    /**
     * 入场时间，ISO8601格式：
     * yyyy-MM-ddTHH:mm:ss+当前时区，例如北京时间：
     * 2018-07-26T15:00:00+08:00
     */
    private String inTime;

    /**
     * 停车时长，(x天x小时x分钟)
     * 此字段仅用作展示，如要获取具体时间可根据返回的入场时间计算，展示依据接口传递的
     */
    private String parkTime;

    /**
     * 车辆所在停车库唯一标识
     */
    private String parkSyscode;
    /**
     * 车辆所在停车库名称
     */
    private String parkName;

    /**
     * 车牌号码，和卡号信息不一定同时存在
     */
    private String plateNo;


}
