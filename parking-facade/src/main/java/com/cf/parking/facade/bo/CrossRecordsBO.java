package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author lpy
 * @date 2023-03-28
 * @description 过车记录表
 */
@Data
@Accessors(chain = true)
public class CrossRecordsBO {


    /**
     * 过车记录ID
     */
    private Long crossRecordsId;

    /**
     * 停车库名称
     */
    private String parkingTitle;

    /**
     * 车牌号
     */
    private String plateNo;

    /**
     * 是否出场 0-进场，1-出场
     */
    private String vehicleOut;

    /**
     * 过车记录唯一标识
     */
    private String crossRecordSyscode;

    /**
     * 停车库唯一标识
     */
    private String parkSyscode;

    /**
     * 出入口唯一标识
     */
    private String entranceSyscode;

    /**
     * 出入口名称
     */
    private String entranceName;

    /**
     * 通行时间
     */
    private String crossTime;


}
