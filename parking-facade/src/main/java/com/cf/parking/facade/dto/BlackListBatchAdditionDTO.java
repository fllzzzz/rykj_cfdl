package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author lpy
 * @date 2023-03-27 09:43:43
 * @description 黑名单记录表
 */
@Data
@Accessors(chain = true)
public class BlackListBatchAdditionDTO {


    /**
     * 黑名单id
     */
    private Long blackListId;


    /**
     * 是否同步 0:未同步  1：已同步
     */
    private Integer isAsync;

    /**
     * 工号
     */
    private String jobNumber;

    /**
     * 原因
     */
    private String joinReason;

    /**
     * alarmSyscode
     */
    private String alarmSyscode;

    /**
     * 车牌号
     */
    private String plateNo;

    /**
     * 姓名
     */
    private String name;

}
