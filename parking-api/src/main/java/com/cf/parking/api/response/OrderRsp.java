package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @author whx
 * @date 2022/11/11
 */
@Data
public class OrderRsp {
	@ApiModelProperty(value = "订单号")
	private Long parkingOrderId;

	@ApiModelProperty(value = "起始地省")
	private String startProvince;

	@ApiModelProperty(value = "起始地市")
	private String startCity;

	@ApiModelProperty(value = "起始地乡")
	private String startCounty;

	@ApiModelProperty(value = "起始地详细地址")
	private String startAddress;

	@ApiModelProperty(value = "目的地省")
	private String destProvince;

	@ApiModelProperty(value = "目的地市")
	private String destCity;

	@ApiModelProperty(value = "目的地区")
	private String destCounty;

	@ApiModelProperty(value = "目的地详细地址")
	private String destAddress;

	@ApiModelProperty(value = "出发时间")
	private Date orderTime;

	@ApiModelProperty(value = "乘客人数")
	private Integer passengerNum;

	@ApiModelProperty(value = "备注")
	private String remark;

	@ApiModelProperty(value = "订单状态")
	private Integer orderState;

	@ApiModelProperty(value = "起始地经度")
	private BigDecimal startLongitude;

	@ApiModelProperty(value = "起始地纬度")
	private BigDecimal startLatitude;

	@ApiModelProperty(value = "目的地经度")
	private BigDecimal destLongitude;

	@ApiModelProperty(value = "目的地纬度")
	private BigDecimal destLatitude;

	@ApiModelProperty(value = "订单距离")
	private BigDecimal distance;

	@ApiModelProperty(value = "时间格式化")
	private String orderTimeShow;
}
