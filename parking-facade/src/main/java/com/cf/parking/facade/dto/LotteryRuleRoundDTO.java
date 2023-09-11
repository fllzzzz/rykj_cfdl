package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号规则-轮数
 * @author
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class LotteryRuleRoundDTO extends PageRequest {
    /** id */
    private Long id;

    /** 轮数名称 */
    private String name;

    /** 状态(0：停用，1：启用) */
    private String state;
}
