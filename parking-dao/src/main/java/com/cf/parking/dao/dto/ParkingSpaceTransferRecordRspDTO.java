package com.cf.parking.dao.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;

import java.util.Date;

/**
 * PC端车位转赠记录返回对象
 * @author
 * @date 2023/10/9
 */
@Data
public class ParkingSpaceTransferRecordRspDTO {
    /** id */
    private Long id;

    /** 转赠停车场编号 */
    private String parkingLotCode;

    /** 转赠停车场名称 */
    private String parkingLotRegion;

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

    //PC端展示属性
    /** 申请人姓名 */
    private String userName;

    /** 申请人工号 */
    private String userJobNumber;

    /** 赠予人工号 */
    private String acceptUserJobNumber;
}
