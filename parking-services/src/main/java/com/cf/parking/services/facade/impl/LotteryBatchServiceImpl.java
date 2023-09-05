package com.cf.parking.services.facade.impl;

import java.util.List;

import com.cf.parking.dao.mapper.LotteryBatchMapper;
import com.cf.parking.facade.facade.LotteryBatchFacade;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 摇号批次Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Service
public class LotteryBatchServiceImpl implements LotteryBatchFacade
{
    @Autowired
    private LotteryBatchMapper lotteryBatchMapper;

    /**
     * 查询摇号批次
     * 
     * @param id 摇号批次主键
     * @return 摇号批次
     */
//    @Override
//    public LotteryBatch selectLotteryBatchById(Long id)
//    {
//        return lotteryBatchMapper.selectLotteryBatchById(id);
//    }

    /**
     * 查询摇号批次列表
     * 
     * @param lotteryBatch 摇号批次
     * @return 摇号批次
     */
//    @Override
//    public List<LotteryBatch> selectLotteryBatchList(LotteryBatch lotteryBatch)
//    {
//        return lotteryBatchMapper.selectLotteryBatchList(lotteryBatch);
//    }

    /**
     * 新增摇号批次
     * 
     * @param lotteryBatch 摇号批次
     * @return 结果
     */
//    @Override
//    public int insertLotteryBatch(LotteryBatch lotteryBatch)
//    {
//        return lotteryBatchMapper.insertLotteryBatch(lotteryBatch);
//    }

    /**
     * 修改摇号批次
     * 
     * @param lotteryBatch 摇号批次
     * @return 结果
     */
//    @Override
//    public int updateLotteryBatch(LotteryBatch lotteryBatch)
//    {
//        return lotteryBatchMapper.updateLotteryBatch(lotteryBatch);
//    }

    /**
     * 批量删除摇号批次
     * 
     * @param ids 需要删除的摇号批次主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryBatchByIds(Long[] ids)
//    {
//        return lotteryBatchMapper.deleteLotteryBatchByIds(ids);
//    }

    /**
     * 删除摇号批次信息
     * 
     * @param id 摇号批次主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryBatchById(Long id)
//    {
//        return lotteryBatchMapper.deleteLotteryBatchById(id);
//    }
}
