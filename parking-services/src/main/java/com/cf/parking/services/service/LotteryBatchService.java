package com.cf.parking.services.service;

import org.springframework.stereotype.Service;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryBatchMapper;
import com.cf.parking.dao.po.LotteryBatchPO;

import javax.annotation.Resource;


@Service
public class LotteryBatchService extends ServiceImpl<LotteryBatchMapper, LotteryBatchPO> implements IService<LotteryBatchPO>{

    @Resource
    private LotteryBatchMapper lotteryBatchMapper;

    /**
     * 将对应批次状态修改为已结束
     * @param batchId
     */
    public Integer archive(Long batchId) {
        LotteryBatchPO po = new LotteryBatchPO();
        po.setId(batchId);
        po.setState("2");
        return lotteryBatchMapper.updateById(po);
    }
}