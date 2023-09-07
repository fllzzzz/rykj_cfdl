package com.cf.parking.facade.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;
import java.util.List;

/**
 * 车辆审核
 * @author
 * @date 2023/9/7
 */
@Data
@Accessors(chain = true)
public class UserVerifyOptDTO {

    /** 批量审核的ids */
    private List<Long> ids;

    /** id */
    private Long id;

    /**
     * userID
     *
     */
    private Long userId;

    /**
     * 申请人
     */
    private String userName;

    /**
     * 车牌号
     */
    private String plateNo;

    /**
     * 车辆照片
     */
    private String vehicleImg;

    /**
     * 行驶证照片path
     */
    private String drivingPermitImg;

    /**
     * 驾驶证照片path
     */
    private String drivingLicenseImg;

    /**
     * 状态(0:待审核，1:审核失败,2:审核通过 3:审核不通过)
     */
    private String state;

    /**
     * 审核意见
     */
    private String reason;

    /**
     * 申请日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;
}
