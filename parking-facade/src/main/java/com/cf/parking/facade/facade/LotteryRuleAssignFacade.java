package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.LotteryRuleAssignBO;
import com.cf.parking.facade.dto.LotteryRuleAssignDTO;
import com.cf.parking.facade.dto.LotteryRuleAssignOptDTO;
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

    /**
     * 新增摇号规则-停车场分配
     * @param dto
     * @return
     */
    Integer add(LotteryRuleAssignOptDTO dto);

    /**
     * 修改摇号规则-停车场分配
     * @param dto
     * @return
     */
    Integer update(LotteryRuleAssignOptDTO dto);

    /**
     * 删除摇号规则-停车场分配
     * @param id
     * @return
     */
    Integer deleteById(Long id);
}
