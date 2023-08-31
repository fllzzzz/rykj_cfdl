package com.cf.parking.dao.po;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;


import java.util.Date;

import com.baomidou.mybatisplus.annotation.TableName;

/**
 * @author csy
 * @date 2023-03-27 09:16:26
 *
 * @description 僵尸车扫描记录表
 */
@Data
@TableName("not_parking_record")
@Accessors(chain = true)
public class NotParkingRecordPO  {


    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 扫描结束时间
     */
    private Date endTm;

    /**
     * 是否删除，0：未删除  1：已删除
     */
    private Integer isDelete;

    /**
     * 记录id
     */
    @TableId(value = "record_id", type =  IdType.INPUT )
    private Long recordId;

    /**
     * 扫描开始时间
     */
    private Date startTm;

    /**
     * 更新时间
     */
    private Date updateTm;
}
