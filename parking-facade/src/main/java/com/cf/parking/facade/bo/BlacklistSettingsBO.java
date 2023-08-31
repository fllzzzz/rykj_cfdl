package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author lpy
 * @date 2023-03-27 10:29:20
 * @description 黑名单设置表
 */
@Data
@Accessors(chain = true)
public class BlacklistSettingsBO {


    /**
     * id
     */
    private Long blacklistSettingsId;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 累计次数
     */
    private Integer cumulativeNumber;

    /**
     * 无出入记录时长
     */
    private Integer noRecordInTime;

}
