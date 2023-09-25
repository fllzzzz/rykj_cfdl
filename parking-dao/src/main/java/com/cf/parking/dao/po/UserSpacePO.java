package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author lpy
 * @date 2023-03-27 16:56:05
 * @description 用户车位表
 */
@Data
@TableName("user_space")
@Accessors(chain = true)
public class UserSpacePO {


    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 结束时间
     */
    private Date endDate;

    /**
     * 工号
     */
    private String jobNumber;

    /**
     * 姓名
     */
    private String name;

    /**
     * 所属车场名(多个逗号分割)
     */
    private String parkingLot;

    /**
     * 车牌号
     */
    private String plateNo;

    /**
     * 开始时间
     */
    private Date startDate;

    /**
     * 更新时间
     */
    private Date updateTm;

    /**
     * 车位管理ID
     */
    @TableId(value = "user_space_id", type = IdType.INPUT)
    private Long userSpaceId;

    /** 状态（0：未同步；1：同步成功；2：同步失败） */
    private String state;
    
    /** 定时器执行时间 */
    private String scheduleDate;

    /**
     * 期号
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date batchNum;

    /**
     * 摇号轮数id
     */
    private Long roundId;
    
    /**
     * 批次ID
     */
    private Long batchId;
    
    /**
     * 同步失败原因
     */
    private String failReason;
    
    /**
     * 类型，1摇号，2默认
     */
    private Integer type;
}
