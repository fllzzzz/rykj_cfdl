package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;


import java.util.Date;

/**
 * @author whx
 * @date 2023-03-27 09:31:03
 * @description 排班记录表
 */
@Data
@TableName("schedule_data")
@Accessors(chain = true)
public class ScheduleDataPO {

    /**
     * 排班记录ID
     */
    @TableId(value = "schedule_data_id", type = IdType.INPUT)
    private Long scheduleDataId;

    /**
     * 排班日期
     */
    private Date shiftDate;

    /**
     * 开始时间
     */
    private String startTime;

    /**
     * 结束时间
     */
    private String endTime;

    /**
     * 时长
     */
    private String hours;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;
}
