package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author: lpy
 * @Date: 2023/03/27
 */
@Data
@Accessors(chain = true)
public class UserSpaceSyncDTO implements Serializable {
    /**
     * sysCodes
     */
    private String parkSyscodes;

    /**
     * pageNo
     */
    private Long pageNo;

    /**
     * pageSize
     */
    private Long pageSize;

    /**
     * 截止时间
     */
    private String endTime;

}
