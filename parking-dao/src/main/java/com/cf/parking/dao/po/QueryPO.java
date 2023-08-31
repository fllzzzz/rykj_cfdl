package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.TableField;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @author: lpy
 * @Date: 2022/11/10
 */
@Data
@Accessors(chain = true)
public class QueryPO {
    /**
     * 乘客起始地经度
     */
    @TableField(exist = false)
    private BigDecimal passengerStartLongitude;

    /**
     * 乘客起始地纬度
     */
    @TableField(exist = false)
    private BigDecimal passengerStartLatitude;
    /**
     * 乘客目的地经度
     */
    @TableField(exist = false)
    private BigDecimal passengerDestLongitude;

    /**
     * 乘客目的地纬度
     */
    @TableField(exist = false)
    private BigDecimal passengerDestLatitude;

    /**
     * 订单状态
     */
    private Integer orderState;

}
