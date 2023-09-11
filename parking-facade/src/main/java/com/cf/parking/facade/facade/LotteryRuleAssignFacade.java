package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.LotteryRuleAssignBO;
import com.cf.parking.facade.dto.LotteryRuleAssignDTO;
import com.cf.support.result.PageResponse;

import java.util.List;


/**
 * 摇号规则-停车场分配Service接口
 * 
 * @author
 * @date 2023-09-05
 */
public interface LotteryRuleAssignFacade
{

    /**
     * 查询摇号规则-停车场分配列表
     * @param dto
     * @return
     */
    PageResponse<LotteryRuleAssignBO> getLotteryRuleAssignList(LotteryRuleAssignDTO dto);
}
