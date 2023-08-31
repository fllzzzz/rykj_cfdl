package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

@TableName("order_state_record")
@Data
@Accessors(chain = true)
public class OrderStateRecordPO {
    /**
     * 订单状态id
     */
    @TableId(value = "order_state_record_id", type = IdType.AUTO)
    private Long orderStateRecordId;

    /**
     * 订单号
     */
    private Long parkingOrderId;

    /**
     * 乘客类型 1：乘客 2：司机
     */
    private Integer userType;

    /**
     * 用户ID
     */
    private Long userId;

    /**
     * 操作类型(
     1:司机提交订单(未开始),2:司机确认同行(进行中),3:乘客到达目的地(已完成),
     4:过期自动取消,5:司机取消订单,6:确认同行后被乘客取消)
     */
    private Integer optType;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;

}