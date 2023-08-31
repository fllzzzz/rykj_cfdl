package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

@TableName("order_peer")
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
public class OrderPeerPO {
    /**
     * 记录ID
     */
    @TableId(value = "order_peer_id", type = IdType.INPUT)
    private Long orderPeerId;

    /**
     * 订单号
     */
    private Long parkingOrderId;

    /**
     * 乘客ID
     */
    private Long userId;

    /**
     * 乘客工号
     */
    private String jobNumber;

    /**
     * 乘客姓名
     */
    private String name;

    /**
     * 出发时间
     */
    private Date orderTime;

    /**
     * 同行状态(`
     1. 已请求：请求搭车，司机未点确认同行
     2. 取消请求：乘客自己取消请求搭车
     3. 未成单： 司机确认同行其他乘客
     4. 已确认：司机确认同行
     5. 已取消：司机取消订单
     6. 已结束：乘客到达目的地)
     */
    private Integer recordState;

    /**
     * 乘客出发经度
     */
    private BigDecimal passengerStartLongitude;

    /**
     * 乘客出发纬度
     */
    private BigDecimal passengerStartLatitude;

    /**
     * 乘客到达经度
     */
    private BigDecimal passengerDestLongitude;

    /**
     * 乘客到达经度
     */
    private BigDecimal passengerDestLatitude;

    /**
     * 乘客出发地距离乘客到达地距离
     */
    private BigDecimal startDestDistance;

    /**
     * 乘客上车地距离起始地距离
     */
    private BigDecimal startDistance;

    /**
     * 乘客下车地距离目的地距离
     */
    private BigDecimal destDistance;

    /**
     * 乘客上车时间
     */
    private Date getInTm;

    /**
     * 乘客下车时间
     */
    private Date getOffTm;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;
}