package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;


import java.util.Date;

/**
 * @author whx
 * @date 2023-03-31 10:13:21
 * @description 钉钉通知记录表
 */
@Data
@TableName("ding_notice_record")
@Accessors(chain = true)
public class DingNoticeRecordPO {

    /**
     * 钉钉通知记录id
     */
    @TableId(value = "ding_notice_record_id", type = IdType.INPUT)
    private Long dingNoticeRecordId;

    /**
     * 工号
     */
    private String jobNumber;

    /**
     * 通知内容
     */
    private String message;

    /**
     * 是否通知，0：未通知 1：已通知
     */
    private Integer status;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;
}
