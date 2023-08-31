package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
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


    /**
     * userID
     */
    @TableId(value = "user_id", type = IdType.INPUT)
    private Long userId;

    /**
     * 车牌号
     */
    private String plateNo;

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
    private Integer state;

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
