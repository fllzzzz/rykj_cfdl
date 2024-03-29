package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 停车场主对象 parking_lot
 * 
 * @author
 * @date 2023-09-05
 */
@Data
@TableName("parking_lot")
@Accessors(chain = true)
public class ParkingLotPO
{
    /** id */
    @TableId(value = "id", type =  IdType.INPUT )
    private Long id;

    /** parentId */
    private Long parentId;

    /** 区域 */
    private String region;

    /** 区域编号 */
    private String regionCode;

    /** 车位数量 */
    private Long amount;

    /** 类型(0：可摇号，1：不可摇号) */
    private String type;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

    /** 备注 */
    private String remark;

    /** 图片信息，由于太大，为了接口响应速度这里在使用queryWrapper时不从数据库里查 */
    @TableField(exist=false)
    private String imageInfo;

}
