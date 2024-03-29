package com.cf.parking.facade.bo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 车位转赠记录
 * @author
 * @date 2023/9/5
 */
@Data
public class ParkingSpaceTransferRecordBO {
    /** id */
    private Long id;

    /** 转赠停车场编号*/
    private String parkingLotCode;

    /** 转赠停车场区域 */
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
