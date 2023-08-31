package com.cf.parking.api.response;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author lpy
 * @date 2023-03-27 09:43:43
 * @description 黑名单记录表
 */
@Data
@Accessors(chain = true)
public class BlackListPageRsp {


    /**
     * 黑名单id
     */
    private Long blackListId;

    /**
     * 创建时间
     */
    private String createTm;

    /**
     * 是否删除，0：未删除  1：已删除
     */
    private Integer isDelete;

    /**
     * 工号
     */
    private String jobNumber;

    /**
     * alarmSyscode
     */
    private String alarmSyscode;

    /**
     * 姓名
     */
    private String name;

    /**
     * 车牌号
     */
    private String plateNo;

    /**
     * 加入原因
     */
    private String joinReason;

}
