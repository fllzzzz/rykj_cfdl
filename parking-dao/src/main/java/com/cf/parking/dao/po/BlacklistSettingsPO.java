package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;


import java.util.Date;

import com.baomidou.mybatisplus.annotation.TableName;

/**
 * @author lpy
 * @date 2023-03-27 10:29:20
 * @description 黑名单设置表
 */
@Data
@TableName("blacklist_settings")
@Accessors(chain = true)
public class BlacklistSettingsPO {


    /**
     * id
     */
    @TableId(value = "blacklist_settings_id", type = IdType.INPUT)
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

    /**
     * 更新时间
     */
    private Date updateTm;
}
