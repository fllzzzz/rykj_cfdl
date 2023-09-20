package com.cf.parking.dao.po;

import java.util.Date;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 车位转赠记录对象 parking_space_transfer_record
 * 
 * @author
 * @date 2023-09-05
 */
@Data
@TableName("parking_space_transfer_record")
@Accessors(chain = true)
public class ParkingSpaceTransferRecordPO
{
    /** id */
    @TableId(value = "id", type =  IdType.INPUT )
    private Long id;

    /** 转赠停车场编号 */
    private String parkingLotCode;

    /** 申请人userId */
    private Long userId;

    /** 赠予人userId */
    private Long acceptUserId;

    /** 赠予人姓名 */
    private String acceptUserName;

    /** 转赠有效开始日期 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validStartDate;

    /** 转赠有效截止日期 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validEndDate;

    /** 创建时间 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date createTm;

    /** 更新时间 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date updateTm;

}
