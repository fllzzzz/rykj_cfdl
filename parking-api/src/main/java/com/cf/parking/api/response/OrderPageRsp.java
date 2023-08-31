package com.cf.parking.api.response;

import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @author whx
 * @date 2022/10/19
 */
@Data
@Accessors(chain = true)
public class OrderPageRsp {
    private Long parkingOrderId;

    private Long userId;

    private String jobNumber;

    private String name;

    private String startProvince;

    private String startCity;

	private String startCounty;

	private String startAddress;

	private String destProvince;

	private String destCity;

	private String destCounty;

	private String destAddress;

	private Date orderTime;

	private Integer passengerNum;

	private String remark;

	private Integer noticed;

	private Integer orderState;
	/**
	 * 是否发送过请求
	 */
	private Integer requestFlag;
	/**
	 * 头像url
	 */
	private String avatar;
	/**
	 * 开车评分(1:*,2:**,:3:***,4:****,5:*****)
	 */
	private Integer driveMark;
	/**
	 * 出发时间
	 */
	private String orderTimeShow;


	/**
	 * 起始地经度
	 */
	private BigDecimal startLongitude;

	/**
	 * 起始地纬度
	 */
	private BigDecimal startLatitude;

	/**
	 * 目的地经度
	 */
	private BigDecimal destLongitude;

	/**
	 * 目的地纬度
	 */
	private BigDecimal destLatitude;

	/**
	 * 行驶距离
	 */
	private BigDecimal distance;

	/**
	 * 乘客选择起始点距离司机起始点距离
	 */
	private String startDistanceShow;

	/**
	 * 乘客选择下车点距离司机下车距离
	 */
	private String destDistanceShow;
}
