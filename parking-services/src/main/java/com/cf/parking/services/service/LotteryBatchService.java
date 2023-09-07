package com.cf.parking.services.service;

import org.springframework.stereotype.Service;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryBatchMapper;
import com.cf.parking.dao.po.LotteryBatchPO;



@Service
public class LotteryBatchService extends ServiceImpl<LotteryBatchMapper, LotteryBatchPO> implements IService<LotteryBatchPO>{

	
}
