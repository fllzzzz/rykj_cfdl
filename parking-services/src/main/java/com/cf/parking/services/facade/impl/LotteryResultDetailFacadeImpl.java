package com.cf.parking.services.facade.impl;


import com.cf.parking.dao.mapper.LotteryResultDetailMapper;
import com.cf.parking.facade.facade.LotteryResultDetailFacade;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * 摇号结果详情Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Service
public class LotteryResultDetailFacadeImpl implements LotteryResultDetailFacade
{
    @Resource
    private LotteryResultDetailMapper lotteryResultDetailMapper;

}
