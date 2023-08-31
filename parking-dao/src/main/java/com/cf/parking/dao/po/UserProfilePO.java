package com.cf.parking.dao.po;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @author csy
 * @date 2022-11-10 15:35:14
 * @description 小程序用户信息表
 */
@Data
@TableName("user_profile")
@Accessors(chain = true)
public class UserProfilePO {


    /**
     * userID
     */
    @TableId(value = "user_id", type = IdType.INPUT)
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
     * 搭车总公里数
     */
    private BigDecimal rideTotalKilometers;

    /**
     * 开车总公里数
     */
    private BigDecimal driveTotalKilometers;

    /**
     * 搭车评分次数
     */
    private Integer rideEvaluateNum;

    /**
     * 开车评分次数
     */
    private Integer driveEvaluateNum;

    /**
     * 搭车总评分
     */
    private Integer rideTotalMark;

    /**
     * 开车总评分
     */
    private Integer driveTotalMark;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;
}
