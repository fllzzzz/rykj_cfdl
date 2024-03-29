package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.LotteryRuleRoundPO;

import java.util.List;

/**
 * 摇号规则-轮数Mapper接口
 * 
 * @author
 * @date 2023-09-05
 */
public interface LotteryRuleRoundMapper extends BaseMapper<LotteryRuleRoundPO>
{

    /**
     * 查询摇号规则-轮数中的id-name的列表
     * @return
     */
    List<LotteryRuleRoundPO> selectBaseList();

}
