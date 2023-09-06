package com.cf.parking.services.facade.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.cf.parking.dao.mapper.LotteryRuleRoundMapper;
import com.cf.parking.dao.po.LotteryRuleRoundPO;
import com.cf.parking.facade.facade.LotteryRuleRoundFacade;
import org.apache.commons.lang3.StringUtils;
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
    private LotteryRuleRoundMapper mapper;

    /** 根据摇号轮数id转换成对应的轮数名称并进行拼接*/
    public String getNameByRoundId(String roundId){
        String name = null;
        if (StringUtils.isBlank(roundId)){
            return name;
        }

        if (roundId.contains(",")){
            String[] splitIds = roundId.split(",");
            List<String> nameList = new ArrayList<>();
            for (String splitId : splitIds) {
                LotteryRuleRoundPO roundPO = mapper.selectById(Long.parseLong(splitId));
                if (StringUtils.isNotBlank(roundPO.getName())){
                    nameList.add(roundPO.getName());
                }
            }
            name = nameList.stream().collect(Collectors.joining(","));
        }else {
            LotteryRuleRoundPO roundPO = mapper.selectById(Long.parseLong(roundId));
            name = StringUtils.isNotBlank(roundPO.getName()) ?  roundPO.getName() :  null;
        }

        return name;
    }

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
