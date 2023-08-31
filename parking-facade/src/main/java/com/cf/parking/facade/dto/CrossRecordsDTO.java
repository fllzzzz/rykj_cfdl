package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author whx
 * @date 2023/3/30
 */
@Data
@Accessors(chain = true)
public class CrossRecordsDTO implements Serializable {

	/**
	 * 过车记录ID
	 */
	private Long crossRecordsId;

	/**
	 * 过车记录唯一标识
	 */
	private String crossRecordSyscode;

	/**
	 * 出入口名称
	 */
	private String entranceName;

	/**
	 * 出入口唯一标识
	 */
	private String entranceSyscode;

	/**
	 * 停车库唯一标识
	 */
	private String parkSyscode;

	/**
	 * 是否出场 0-进场，1-出场
	 */
	private Integer vehicleOut;

	/**
	 * 车牌号
	 */
	private String plateNo;

	/**
	 * 放行模式 0-禁止放行，1-固定车包期，2-临时车入场，3-预约车入场，10-离线出场，11-缴费出场，12-预付费出场，13-免费出场，30-非法卡不放行，31-手动放行，32-特殊车辆放行，33-节假日放行，35-群组放行，36-遥控器开闸
	 */
	private Integer releaseMode;

	/**
	 * 放行原因，100-固定车自动放行 101-临时车自动放行 102-预约车自动放行 103-一户多车自动放行
	 */
	private Integer releaseReason;

	/**
	 * 放行结果 0-未放行 1-正常放行 2-离线放行
	 */
	private Integer releaseResult;

	/**
	 * 放行方式 10-未开闸 11-自动开闸 12-人工/人工开闸 13-遥控器开闸
	 */
	private Integer releaseWay;

	/**
	 * 停车库名称
	 */
	private String parkName;

	/**
	 * 通行时间
	 */
	private String crossTime;

}
