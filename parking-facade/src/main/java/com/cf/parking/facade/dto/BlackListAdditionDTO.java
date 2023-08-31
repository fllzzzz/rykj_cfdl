package com.cf.parking.facade.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * @author: lpy
 * @Date: 2023/03/29
 */
@Data
public class BlackListAdditionDTO implements Serializable {

    /**
     * plateNo
     */
    private String plateNo;

    /**
     * 可不传 eg:2018-07-26T15:00:00+08:00
     */
    private String endTime;
}
