package com.cf.parking.services.facade.impl;

import java.util.List;

import com.cf.parking.dao.mapper.LotteryRuleRoundMapper;
import com.cf.parking.facade.facade.LotteryRuleRoundFacade;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 摇号规则-轮数Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Service
public class LotteryRuleRoundServiceImpl implements LotteryRuleRoundFacade
{
    @Autowired
    private LotteryRuleRoundMapper lotteryRuleRoundMapper;

    /**
     * 查询摇号规则-轮数
     * 
     * @param id 摇号规则-轮数主键
     * @return 摇号规则-轮数
     */
//    @Override
//    public LotteryRuleRound selectLotteryRuleRoundById(Long id)
//    {
//        return lotteryRuleRoundMapper.selectLotteryRuleRoundById(id);
//    }

    /**
     * 查询摇号规则-轮数列表
     * 
     * @param lotteryRuleRound 摇号规则-轮数
     * @return 摇号规则-轮数
     */
//    @Override
//    public List<LotteryRuleRound> selectLotteryRuleRoundList(LotteryRuleRound lotteryRuleRound)
//    {
//        return lotteryRuleRoundMapper.selectLotteryRuleRoundList(lotteryRuleRound);
//    }

    /**
     * 新增摇号规则-轮数
     * 
     * @param lotteryRuleRound 摇号规则-轮数
     * @return 结果
     */
//    @Override
//    public int insertLotteryRuleRound(LotteryRuleRound lotteryRuleRound)
//    {
//        return lotteryRuleRoundMapper.insertLotteryRuleRound(lotteryRuleRound);
//    }

    /**
     * 修改摇号规则-轮数
     * 
     * @param lotteryRuleRound 摇号规则-轮数
     * @return 结果
     */
//    @Override
//    public int updateLotteryRuleRound(LotteryRuleRound lotteryRuleRound)
//    {
//        return lotteryRuleRoundMapper.updateLotteryRuleRound(lotteryRuleRound);
//    }

    /**
     * 批量删除摇号规则-轮数
     * 
     * @param ids 需要删除的摇号规则-轮数主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryRuleRoundByIds(Long[] ids)
//    {
//        return lotteryRuleRoundMapper.deleteLotteryRuleRoundByIds(ids);
//    }

    /**
     * 删除摇号规则-轮数信息
     * 
     * @param id 摇号规则-轮数主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryRuleRoundById(Long id)
//    {
//        return lotteryRuleRoundMapper.deleteLotteryRuleRoundById(id);
//    }
}
