package com.cf.parking.api.response;

import lombok.Data;

import java.util.Date;

/**
 * 停车场主对象
 * @author
 * @date 2023/09/05
 */
@Data
public class ParkingLotPageRsp {

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

    /** 是否同步闸机系统成功，默认0，0：未同步，1：同步成功 */
    private String isSync;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
