package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Data
@Accessors(chain = true)
public class UserProfileDTO {
    private Long userId;

    /**
     * 手机号
     */
    private String mobile;

    /**
     * 姓名
     */
    private String name;

    /**
     * 工号
     */
    private String jobNumber;

    /**
     * 头像url(钉钉接口返回全路径)
     */
    private String avatar;

    /**
     * 开车次数
     */
    private Integer driveTime;

    /**
     * 搭车次数
     */
    private Integer rideTime;

    /**
     * 开车评分(1:*,2:**,:3:***,4:****,5:*****)
     */
    private Integer driveMark;

    /**
     * 搭车评分(1:*,2:**,:3:***,4:****,5:*****)
     */
    private Integer rideMark;

    /**
     * 用户总积分
     */
    private Integer totalScore;

    /**
     * 居住地
     */
    private String livePlace;

    /**
     * 车牌号
     */
    private String plateNo;
}
