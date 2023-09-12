package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.LotteryRuleRoundBO;
import com.cf.parking.facade.bo.LotteryRuleRoundBaseBO;
import com.cf.parking.facade.dto.LotteryRuleRoundDTO;
import com.cf.parking.facade.dto.LotteryRuleRoundOptDTO;
import com.cf.support.result.PageResponse;

import java.util.List;


/**
 * 摇号规则-轮数Service接口
 * 
 * @author
 * @date 2023-09-05
 */
public interface LotteryRuleRoundFacade
{

    /**
     * 查询摇号规则-轮数中的id—name名称列表
     * @return
     */
    List<LotteryRuleRoundBaseBO> selectBaseList();

    /**
     * 查询摇号规则-轮数列表
     * @param dto
     * @return
     */
    PageResponse<LotteryRuleRoundBO> getLotteryRuleRoundList(LotteryRuleRoundDTO dto);

    /**
     * 新增摇号规则-轮数
     * @param dto
     * @return
     */
    Integer add(LotteryRuleRoundOptDTO dto);

    /**
     * 修改摇号规则-轮数
     * @param dto
     * @return
     */
    Integer update(LotteryRuleRoundOptDTO dto);

    /**
     * 删除摇号规则-轮数
     * @param id
     * @return
     */
    Integer deleteById(Long id);
}
