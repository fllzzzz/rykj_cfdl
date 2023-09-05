package com.cf.parking.services.facade.impl;

import java.util.List;

import com.cf.parking.dao.mapper.LotteryResultMapper;
import com.cf.parking.facade.facade.LotteryResultFacade;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 摇号结果Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Service
public class LotteryResultServiceImpl implements LotteryResultFacade
{
    @Autowired
    private LotteryResultMapper lotteryResultMapper;

    /**
     * 查询摇号结果
     * 
     * @param id 摇号结果主键
     * @return 摇号结果
     */
//    @Override
//    public LotteryResult selectLotteryResultById(Long id)
//    {
//        return lotteryResultMapper.selectLotteryResultById(id);
//    }

    /**
     * 查询摇号结果列表
     * 
     * @param lotteryResult 摇号结果
     * @return 摇号结果
     */
//    @Override
//    public List<LotteryResult> selectLotteryResultList(LotteryResult lotteryResult)
//    {
//        return lotteryResultMapper.selectLotteryResultList(lotteryResult);
//    }

    /**
     * 新增摇号结果
     * 
     * @param lotteryResult 摇号结果
     * @return 结果
     */
//    @Override
//    public int insertLotteryResult(LotteryResult lotteryResult)
//    {
//        return lotteryResultMapper.insertLotteryResult(lotteryResult);
//    }

    /**
     * 修改摇号结果
     * 
     * @param lotteryResult 摇号结果
     * @return 结果
     */
//    @Override
//    public int updateLotteryResult(LotteryResult lotteryResult)
//    {
//        return lotteryResultMapper.updateLotteryResult(lotteryResult);
//    }

    /**
     * 批量删除摇号结果
     * 
     * @param ids 需要删除的摇号结果主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryResultByIds(Long[] ids)
//    {
//        return lotteryResultMapper.deleteLotteryResultByIds(ids);
//    }

    /**
     * 删除摇号结果信息
     * 
     * @param id 摇号结果主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryResultById(Long id)
//    {
//        return lotteryResultMapper.deleteLotteryResultById(id);
//    }
}
