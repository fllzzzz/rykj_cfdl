package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author whx
 * @date 2022/10/19
 */
@Data
@Accessors(chain = true)
public class PendingRsp {
    private Long parkingOrderId;

    private Date orderTime;

    @ApiModelProperty(value = "起始地")
    private String startAddress;

    @ApiModelProperty(value = "目的地")
    private String destAddress;

    @ApiModelProperty(value = "时间格式化")
    private String orderTimeShow;

    @ApiModelProperty(value = "订单状态消息")
    private String orderStateMsg;
}
