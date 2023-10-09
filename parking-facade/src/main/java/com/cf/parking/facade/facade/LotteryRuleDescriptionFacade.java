package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.LotteryRuleDescriptionBO;
import com.cf.parking.facade.dto.LotteryRuleDescriptionOptDTO;

/**
 * 摇号规则描述
 * @author
 * @date 2023/10/8
 */
public interface LotteryRuleDescriptionFacade {

    /**
     * 查询唯一的规则描述记录
     * @return
     */
    LotteryRuleDescriptionBO getDescription();

    /**
     * 编辑摇号规则描述信息
     * @param dto
     * @return
     */
    Integer edit(LotteryRuleDescriptionOptDTO dto);
}
