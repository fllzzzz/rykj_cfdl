package com.cf.parking.facade.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * @author: lpy
 * @Date: 2023/03/30
 */
@Data
public class BlackListAlarmSyscodeDTO implements Serializable {
    private String alarmSyscode;

    private String plateNo;

    private String cardNo;
}
