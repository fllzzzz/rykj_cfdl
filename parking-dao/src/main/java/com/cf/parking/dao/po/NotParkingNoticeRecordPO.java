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
 * @date 2023-03-29 16:41:26
 *
 * @description 不停车通知记录表
 */
@Data
@TableName("not_parking_notice_record")
@Accessors(chain = true)
public class NotParkingNoticeRecordPO  {


    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 是否删除，0：未删除  1：已删除
     */
    private Integer isDelete;

    /**
     * 工号
     */
    private String jobNumber;

    /**
     * 记录id
     */
    @TableId(value = "not_parking_notice_record_id", type =  IdType.INPUT )
    private Long notParkingNoticeRecordId;

    /**
     * 通知次数
     */
    private Integer noticeNumber;

    /**
     * 更新时间
     */
    private Date updateTm;
}
