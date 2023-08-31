package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author lpy
 * @date 2023-03-27 09:43:43
 * @description 黑名单记录表
 */
@Data
@TableName("black_list")
@Accessors(chain = true)
public class BlackListPO {


    /**
     * 黑名单id
     */
    @TableId(value = "black_list_id", type = IdType.INPUT)
    private Long blackListId;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 是否删除，0：未删除  1：已删除
     */
    private Integer isDelete;

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
     * 姓名
     */
    private String name;

    /**
     * 车牌号
     */
    private String plateNo;

    /**
     * 更新时间
     */
    private Date updateTm;
}
