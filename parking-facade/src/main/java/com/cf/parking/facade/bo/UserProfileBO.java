package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @author whx
 * @date 2022/10/21
 */
@Data
@Accessors(chain = true)
public class UserProfileBO {

	/**
	 * openId
	 */
	private String openId;

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
	 * 搭车总公里数
	 */
	private BigDecimal rideTotalKilometers;

	/**
	 * 开车总公里数
	 */
	private BigDecimal driveTotalKilometers;
}
