package com.cf.parking.facade.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * @author: lpy
 * @Date: 2023/03/27
 */
@Data
public class BlackListBatchDelAlarmSyscodeDTO implements Serializable {
    /**
     * 批量删除的alarmSysCodes
     */
    private String alarmSyscodes;
}
