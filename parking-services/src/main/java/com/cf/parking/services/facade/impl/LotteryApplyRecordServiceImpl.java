package com.cf.parking.services.facade.impl;

import java.util.List;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.cf.parking.dao.mapper.LotteryApplyRecordMapper;
import com.cf.parking.facade.facade.LotteryApplyRecordFacade;
import com.cf.support.result.Result;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 摇号申请记录Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Service
public class LotteryApplyRecordServiceImpl implements LotteryApplyRecordFacade
{
    @Autowired
    private LotteryApplyRecordMapper lotteryApplyRecordMapper;

    /**
     * 查询摇号申请记录
     * 
     * @param id 摇号申请记录主键
     * @return 摇号申请记录
     */
//    @Override
//    public Result selectLotteryApplyRecordById(Long id)
//    {
//        return lotteryApplyRecordMapper.selectLotteryApplyRecordById(id);
//    }

    /**
     * 查询摇号申请记录列表
     * 
     * @param lotteryApplyRecord 摇号申请记录
     * @return 摇号申请记录
     */
//    @Override
//    public List<LotteryApplyRecord> selectLotteryApplyRecordList(LotteryApplyRecord lotteryApplyRecord)
//    {
//        return lotteryApplyRecordMapper.selectLotteryApplyRecordList(lotteryApplyRecord);
//    }

    /**
     * 新增摇号申请记录
     * 
     * @param lotteryApplyRecord 摇号申请记录
     * @return 结果
     */
//    @Override
//    public int insertLotteryApplyRecord(LotteryApplyRecord lotteryApplyRecord)
//    {
//        return lotteryApplyRecordMapper.insertLotteryApplyRecord(lotteryApplyRecord);
//    }

    /**
     * 修改摇号申请记录
     * 
     * @param lotteryApplyRecord 摇号申请记录
     * @return 结果
     */
//    @Override
//    public int updateLotteryApplyRecord(LotteryApplyRecord lotteryApplyRecord)
//    {
//        return lotteryApplyRecordMapper.updateLotteryApplyRecord(lotteryApplyRecord);
//    }

    /**
     * 批量删除摇号申请记录
     * 
     * @param ids 需要删除的摇号申请记录主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryApplyRecordByIds(Long[] ids)
//    {
//        return lotteryApplyRecordMapper.deleteLotteryApplyRecordByIds(ids);
//    }

    /**
     * 删除摇号申请记录信息
     * 
     * @param id 摇号申请记录主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryApplyRecordById(Long id)
//    {
//        return lotteryApplyRecordMapper.deleteLotteryApplyRecordById(id);
//    }
}
