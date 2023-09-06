package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author
 * @date 2023/9/6
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "停车场操作对象（新增/修改）")
public class ParkingLotOptReq {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** 区域 */
    @ApiModelProperty(value = "区域")
    private String region;

    /** 区域编号 */
    @ApiModelProperty(value = "区域编号")
    private String regionCode;

    /** 车位数量 */
    @ApiModelProperty(value = "车位数量")
    private Long amount;

    /** 类型(0：不可摇号，1：可摇号) */
    @ApiModelProperty(value = "类型")
    private String type;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
