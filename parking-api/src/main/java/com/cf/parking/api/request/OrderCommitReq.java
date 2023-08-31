package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;


/**
 * @author lpy
 * @date 2022/10/19
 */

@Data
@ApiModel(description = "提交订单请求实体类")
public class OrderCommitReq {
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

    @ApiModelProperty(value = "出发日期(1:今,2:明,3:后)")
    private Integer orderDate;

    @ApiModelProperty(value = "出发时间")
    private String dateTime;

    @ApiModelProperty(value = "乘客人数")
    private Integer passengerNum;

    @ApiModelProperty(value = "备注")
    private String remark;

    @ApiModelProperty(value = "起始地经度")
    private BigDecimal startLongitude;

    @ApiModelProperty(value = "起始地纬度")
    private BigDecimal startLatitude;

    @ApiModelProperty(value = "目的地经度")
    private BigDecimal destLongitude;

    @ApiModelProperty(value = "目的地纬度")
    private BigDecimal destLatitude;
}
