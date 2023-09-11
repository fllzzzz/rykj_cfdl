package com.cf.parking.facade.bo;

import lombok.Data;

/**
 * 摇号规则-轮数
 * @author
 * @date 2023/9/11
 */
@Data
public class LotteryRuleRoundBaseBO {
    /** id */
    private Long id;

    /** 轮数名称 */
    private String name;
}
