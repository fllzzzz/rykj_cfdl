package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.util.Date;


/**
 * <p>
 * 积分记录表
 * </p>
 *
 * @author zwq
 * @since 2022-10-20
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("score_record")
public class ScoreRecordPO {
    /**
     * 积分记录ID
     */
    @TableId(value = "score_record_id", type = IdType.AUTO)
    private Long scoreRecordId;

    /**
     * 订单号
     */
    private Long parkingOrderId;

    /**
     * 用户ID
     */
    private Long userId;

    /**
     * 姓名
     */
    private String name;

    /**
     * 工号
     */
    private String jobNumber;

    /**
     * 得分详情备注
     */
    private String remark;

    /**
     * 分数(加减积分，直接存入正负）
     */
    private Integer score;

    /**
     * 后台手动增减积分操作者
     */
    private Long adminUserId;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;

    @TableField(exist = false)
    private Integer totalScore;
}