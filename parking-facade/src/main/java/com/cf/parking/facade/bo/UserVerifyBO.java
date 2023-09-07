package com.cf.parking.facade.bo;

import lombok.Data;

import java.util.Date;

/**
 * 车辆审核
 * @author
 * @date 2023/9/7
 */
@Data
public class UserVerifyBO {

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
     * 状态(0:默认，1:待审核，2:审核失败,3:审核成功)
     */
    private String state;

    /**
     * 审核意见
     */
    private String reason;

    /**
     * 申请日期
     */
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;
}
