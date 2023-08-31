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
 * @date 2023-03-29 14:31:06
 *
 * @description 通知记录表
 */
@Data
@TableName("notice_record")
@Accessors(chain = true)
public class NoticeRecordPO  {


    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 入库唯一标识
     */
    private String inRecordSyscode;

    /**
     * 是否删除，0：未删除  1：已删除
     */
    private Integer isDelete;

    /**
     * 工号
     */
    private String jobNumber;

    /**
     * 上次停车时长
     */
    private Integer lastPeekHour;

    /**
     * 通知次数
     */
    private Integer noticeNumber;

    /**
     * 记录id
     */
    @TableId(value = "notice_record_id", type =  IdType.INPUT )
    private Long noticeRecordId;

    /**
     * 更新时间
     */
    private Date updateTm;
}
