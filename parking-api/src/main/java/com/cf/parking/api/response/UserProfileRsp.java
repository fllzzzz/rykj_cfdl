package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;

/**
 * @author whx
 * @date 2022/10/21
 */
@Data
public class UserProfileRsp {

	/**
	 * openId
	 */
	@ApiModelProperty(value = "openId")
	private String openId;

	/**
	 * userID
	 */
	@ApiModelProperty(value = "userID")
	private Long userId;

	/**
	 * 手机号
	 */
	@ApiModelProperty(value = "手机号")
	private String mobile;

	/**
	 * 姓名
	 */
	@ApiModelProperty(value = "姓名")
	private String name;

	/**
	 * 工号
	 */
	@ApiModelProperty(value = "工号")
	private String jobNumber;

	/**
	 * 头像url(钉钉接口返回全路径)
	 */
	@ApiModelProperty(value = "头像url(钉钉接口返回全路径)")
	private String avatar;

	/**
	 * 开车次数
	 */
	@ApiModelProperty(value = "开车次数")
	private Integer driveTime;

	/**
	 * 搭车次数
	 */
	@ApiModelProperty(value = "搭车次数")
	private Integer rideTime;

	/**
	 * 开车评分(1:*,2:**,:3:***,4:****,5:*****)
	 */
	@ApiModelProperty(value = "开车评分(1:*,2:**,:3:***,4:****,5:*****)")
	private Integer driveMark;

	/**
	 * 搭车评分(1:*,2:**,:3:***,4:****,5:*****)
	 */
	@ApiModelProperty(value = "搭车评分(1:*,2:**,:3:***,4:****,5:*****)")
	private Integer rideMark;

	/**
	 * 用户总积分
	 */
	@ApiModelProperty(value = "总积分")
	private Integer totalScore;

	/**
	 * 居住地
	 */
	@ApiModelProperty(value = "居住地")
	private String livePlace;

	/**
	 * 车牌号
	 */
	@ApiModelProperty(value = "车牌号")
	private String plateNo;

	@ApiModelProperty(value = "搭车总公里数")
	private BigDecimal rideTotalKilometers;

	@ApiModelProperty(value = "开车总公里数")
	private BigDecimal driveTotalKilometers;

	/**
	 * 停车场区域
	 * 2023/09/18
	 */
	@ApiModelProperty(value = "停车场名称")
	private String parkingLotRegion;
}