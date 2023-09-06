package com.cf.parking.facade.bo;

import io.swagger.annotations.ApiModelProperty;

import java.util.Date;

/**
 * 停车场
 * @author
 * @date 2023/9/5
 */
public class ParkingLotBO {
    /** id */
    private Long id;

    /** 区域 */
    private String region;

    /** 区域编号 */
    private String regionCode;

    /** 车位数量 */
    private Long amount;

    /** 类型(0：不可摇号，1：可摇号) */
    private String type;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
