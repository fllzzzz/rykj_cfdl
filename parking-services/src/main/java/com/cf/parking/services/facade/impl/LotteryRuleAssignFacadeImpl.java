package com.cf.parking.services.facade.impl;

import java.util.List;

import com.cf.parking.dao.mapper.LotteryRuleAssignMapper;
import com.cf.parking.facade.facade.LotteryRuleAssignFacade;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 摇号规则-停车场分配Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Service
public class LotteryRuleAssignFacadeImpl implements LotteryRuleAssignFacade
{
    @Autowired
    private LotteryRuleAssignMapper lotteryRuleAssignMapper;

    /**
     * 查询摇号规则-停车场分配
     * 
     * @param id 摇号规则-停车场分配主键
     * @return 摇号规则-停车场分配
     */
//    @Override
//    public LotteryRuleAssign selectLotteryRuleAssignById(Long id)
//    {
//        return lotteryRuleAssignMapper.selectLotteryRuleAssignById(id);
//    }

    /**
     * 查询摇号规则-停车场分配列表
     * 
     * @param lotteryRuleAssign 摇号规则-停车场分配
     * @return 摇号规则-停车场分配
     */
//    @Override
//    public List<LotteryRuleAssign> selectLotteryRuleAssignList(LotteryRuleAssign lotteryRuleAssign)
//    {
//        return lotteryRuleAssignMapper.selectLotteryRuleAssignList(lotteryRuleAssign);
//    }

    /**
     * 新增摇号规则-停车场分配
     * 
     * @param lotteryRuleAssign 摇号规则-停车场分配
     * @return 结果
     */
//    @Override
//    public int insertLotteryRuleAssign(LotteryRuleAssign lotteryRuleAssign)
//    {
//        return lotteryRuleAssignMapper.insertLotteryRuleAssign(lotteryRuleAssign);
//    }

    /**
     * 修改摇号规则-停车场分配
     * 
     * @param lotteryRuleAssign 摇号规则-停车场分配
     * @return 结果
     */
//    @Override
//    public int updateLotteryRuleAssign(LotteryRuleAssign lotteryRuleAssign)
//    {
//        return lotteryRuleAssignMapper.updateLotteryRuleAssign(lotteryRuleAssign);
//    }

    /**
     * 批量删除摇号规则-停车场分配
     * 
     * @param ids 需要删除的摇号规则-停车场分配主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryRuleAssignByIds(Long[] ids)
//    {
//        return lotteryRuleAssignMapper.deleteLotteryRuleAssignByIds(ids);
//    }

    /**
     * 删除摇号规则-停车场分配信息
     * 
     * @param id 摇号规则-停车场分配主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryRuleAssignById(Long id)
//    {
//        return lotteryRuleAssignMapper.deleteLotteryRuleAssignById(id);
//    }
}
