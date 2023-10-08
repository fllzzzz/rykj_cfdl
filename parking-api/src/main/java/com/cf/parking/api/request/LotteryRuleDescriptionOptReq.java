package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author
 * @date 2023/10/8
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "摇号规则描述信息查询对象")
public class LotteryRuleDescriptionOptReq {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** 规则描述 */
    @ApiModelProperty(value = "规则描述")
    private String description;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

}
