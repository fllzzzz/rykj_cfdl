package com.cf.parking.services.facade.impl;

import java.util.List;

import com.cf.parking.dao.mapper.LotteryResultDetailMapper;
import com.cf.parking.facade.facade.LotteryResultDetailFacade;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 摇号结果详情Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Service
public class LotteryResultDetailServiceImpl implements LotteryResultDetailFacade
{
    @Autowired
    private LotteryResultDetailMapper lotteryResultDetailMapper;

    /**
     * 查询摇号结果详情
     * 
     * @param id 摇号结果详情主键
     * @return 摇号结果详情
     */
//    @Override
//    public LotteryResultDetail selectLotteryResultDetailById(Long id)
//    {
//        return lotteryResultDetailMapper.selectLotteryResultDetailById(id);
//    }

    /**
     * 查询摇号结果详情列表
     * 
     * @param lotteryResultDetail 摇号结果详情
     * @return 摇号结果详情
     */
//    @Override
//    public List<LotteryResultDetail> selectLotteryResultDetailList(LotteryResultDetail lotteryResultDetail)
//    {
//        return lotteryResultDetailMapper.selectLotteryResultDetailList(lotteryResultDetail);
//    }

    /**
     * 新增摇号结果详情
     * 
     * @param lotteryResultDetail 摇号结果详情
     * @return 结果
     */
//    @Override
//    public int insertLotteryResultDetail(LotteryResultDetail lotteryResultDetail)
//    {
//        return lotteryResultDetailMapper.insertLotteryResultDetail(lotteryResultDetail);
//    }

    /**
     * 修改摇号结果详情
     * 
     * @param lotteryResultDetail 摇号结果详情
     * @return 结果
     */
//    @Override
//    public int updateLotteryResultDetail(LotteryResultDetail lotteryResultDetail)
//    {
//        return lotteryResultDetailMapper.updateLotteryResultDetail(lotteryResultDetail);
//    }

    /**
     * 批量删除摇号结果详情
     * 
     * @param ids 需要删除的摇号结果详情主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryResultDetailByIds(Long[] ids)
//    {
//        return lotteryResultDetailMapper.deleteLotteryResultDetailByIds(ids);
//    }

    /**
     * 删除摇号结果详情信息
     * 
     * @param id 摇号结果详情主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryResultDetailById(Long id)
//    {
//        return lotteryResultDetailMapper.deleteLotteryResultDetailById(id);
//    }
}
