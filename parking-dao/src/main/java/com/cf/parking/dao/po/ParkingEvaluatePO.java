package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.util.Date;

@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("parking_evaluate")
public class ParkingEvaluatePO {
    /**
     * 评价ID
     */
    @TableId(value = "parking_evaluate_id", type = IdType.AUTO)
    private Long parkingEvaluateId;

    /**
     * 用户ID
     */
    private Long userId;

    /**
     * 订单号
     */
    private Long parkingOrderId;

    /**
     * 星级(1:*,2:**,:3:***,4:****,5:*****)
     */
    private Integer level;

    /**
     * 评价
     */
    private String evaluateDesc;

    /**
     * 评价类型(1:评价司机,2:评价乘客)
     */
    private Integer evaluateType;

    /**
     * 被评用户id
     */
    private Long evaluateUserId;

    /**
     * 是否评价(1:是,2:否)
     */
    private Integer isEvaluate;

    /**
     * 自动评价(1:是,2:否)
     */
    private Integer evaluateAuto;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;
}