package com.cf.parking.services.facade.impl;

import java.util.List;

import com.cf.parking.dao.mapper.LotteryBlackListMapper;
import com.cf.parking.facade.facade.LotteryBlackListFacade;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 摇号黑名单Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Service
public class LotteryBlackListServiceImpl implements LotteryBlackListFacade
{
    @Autowired
    private LotteryBlackListMapper lotteryBlackListMapper;

    /**
     * 查询摇号黑名单
     * 
     * @param id 摇号黑名单主键
     * @return 摇号黑名单
     */
//    @Override
//    public LotteryBlackList selectLotteryBlackListById(Long id)
//    {
//        return lotteryBlackListMapper.selectLotteryBlackListById(id);
//    }

    /**
     * 查询摇号黑名单列表
     * 
     * @param lotteryBlackList 摇号黑名单
     * @return 摇号黑名单
     */
//    @Override
//    public List<LotteryBlackList> selectLotteryBlackListList(LotteryBlackList lotteryBlackList)
//    {
//        return lotteryBlackListMapper.selectLotteryBlackListList(lotteryBlackList);
//    }

    /**
     * 新增摇号黑名单
     * 
     * @param lotteryBlackList 摇号黑名单
     * @return 结果
     */
//    @Override
//    public int insertLotteryBlackList(LotteryBlackList lotteryBlackList)
//    {
//        return lotteryBlackListMapper.insertLotteryBlackList(lotteryBlackList);
//    }

    /**
     * 修改摇号黑名单
     * 
     * @param lotteryBlackList 摇号黑名单
     * @return 结果
     */
//    @Override
//    public int updateLotteryBlackList(LotteryBlackList lotteryBlackList)
//    {
//        return lotteryBlackListMapper.updateLotteryBlackList(lotteryBlackList);
//    }

    /**
     * 批量删除摇号黑名单
     * 
     * @param ids 需要删除的摇号黑名单主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryBlackListByIds(Long[] ids)
//    {
//        return lotteryBlackListMapper.deleteLotteryBlackListByIds(ids);
//    }

    /**
     * 删除摇号黑名单信息
     * 
     * @param id 摇号黑名单主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryBlackListById(Long id)
//    {
//        return lotteryBlackListMapper.deleteLotteryBlackListById(id);
//    }
}
