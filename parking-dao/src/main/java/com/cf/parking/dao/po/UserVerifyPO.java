package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;


import java.util.Date;

/**
 * @author whx
 * @date 2022-11-19 16:55:55
 * @description 车主认证表
 */
@Data
@TableName("user_verify")
@Accessors(chain = true)
public class UserVerifyPO {

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
     * 审核失败原因
     */
    private String reason;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;
}
