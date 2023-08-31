package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @Classname UserInfoPO
 * @Date 2022/10/22 9:59
 * @Created by csy
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
public class UserInfoPO {
    /**
     * userID
     */
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
    /**
     * 钉钉userID
     */
    private String openId;

    /**
     * 1-正常
     */
    private Integer state;

    /**
     * 最后活跃时间
     */
    private Date lastActiveAt;

}
