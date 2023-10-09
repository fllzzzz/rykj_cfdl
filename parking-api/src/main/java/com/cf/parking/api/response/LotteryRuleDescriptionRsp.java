package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import lombok.Data;

import java.util.Date;

/**
 * @author
 * @date 2023/10/8
 */
@Data
@ApiModel(description = "摇号规则描述返回结果")
public class LotteryRuleDescriptionRsp {

    /** id */
    private Long id;

    /** 规则描述 */
    private String description;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
