package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

@Data
@Accessors(chain = true)
public class CarInRecordQueryDTO implements Serializable {
    /**
     * 停车库唯一标识
     */
    private String parkSyscode;

    /**
     * 车牌号码，至少包含3个字符
     */
    private String plateNo;

    /**
     * 卡号
     */
    private String cardNo;

    /**
     * 卡号
     */
    private String parkTime;


    /**
     * 停车时长，单位：小时，与“查询开始时间、查询结束时间”互斥使用
     */
    private String startTime;
    /**
     * 与“停车时长”查询互斥使用
     * ISO8601格式：
     * yyyy-MM-ddTHH:mm:ss+当前时区，例如北京时间：
     * 2018-07-26T15:00:00+08:00
     */
    private String endTime;

    /**
     * 目标页码，范围 ( 0 , ~ )，若是范围内数字超过实际最大页码值
     */
    private Integer pageNo = 0;

    /**
     * 每页记录数，范围 ( 0 , 1000 ]
     */
    private Integer pageSize = 1000;

    /**
     * 是否开启车牌相识度匹配查询,0:关闭(不填默认),1:开启
     */
    private String operation;
}
