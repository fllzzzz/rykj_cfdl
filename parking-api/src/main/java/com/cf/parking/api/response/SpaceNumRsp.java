package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author
 * @date 2023/9/20
 */
@Data
@ApiModel(description = "停车场数量饼图")
public class SpaceNumRsp {

    /**
     * 停车场名称
     */
    @ApiModelProperty(value = "停车场名称")
    private String name;

    /**
     * 停车库车位剩余数
     */
    @ApiModelProperty(value = "剩余车位数量")
    private Long value;
}
