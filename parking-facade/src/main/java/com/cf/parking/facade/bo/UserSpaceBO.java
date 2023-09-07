package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author lpy
 * @date 2023/3/29
 */
@Data
@Accessors(chain = true)
public class UserSpaceBO {

    /**
     * 车位管理ID
     */
    private Long userSpaceId;

    /**
     * 所属车场名(多个逗号分割)
     */
    private String parkingLot;

    /**
     * 车牌号
     */
    private String plateNo;

    /**
     * 工号
     */
    private String jobNumber;

    /**
     * 姓名
     */
    private String name;

    /**
     * 开始时间
     */
    private String startDate;

    /**
     * 结束时间
     */
    private String endDate;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;

    /** 状态（0：未同步；1：同步成功；2：同步失败） */
    private String state;
}
