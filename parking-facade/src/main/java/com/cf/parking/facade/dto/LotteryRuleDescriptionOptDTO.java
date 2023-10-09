package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author
 * @date 2023/10/8
 */
@Data
@Accessors(chain = true)
public class LotteryRuleDescriptionOptDTO {

    /** id */
    private Long id;

    /** 规则描述 */
    private String description;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
