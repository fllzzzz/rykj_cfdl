package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 *闸机数据初始化
 */
@Data
@TableName("parking_init")
@Accessors(chain = true)
public class ParkingInitPO {


    /**
     * 黑名单id
     */
    @TableId(value = "id", type = IdType.INPUT)
    private Long id;

    /**
     * 名称
     */
    private String region;

    /**
     * 编码
     */
    private String regionCode;

    /**
     * 备注
     */
    private String remark;


    /**
     * 更新时间
     */
    private Date updateTm;
    
    /**
     * 创建时间
     */
    private Date createTm;
}
