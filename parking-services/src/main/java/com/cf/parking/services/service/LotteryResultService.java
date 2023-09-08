package com.cf.parking.services.service;


import org.springframework.stereotype.Service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryResultMapper;
import com.cf.parking.dao.po.LotteryResultPO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import javax.annotation.Resource;
import java.util.List;

/**
 * @author
 * @date 2023/9/8
 */
@Service
public class LotteryResultService extends ServiceImpl<LotteryResultMapper, LotteryResultPO> implements IService<LotteryResultPO> {

    @Resource
    private LotteryResultMapper mapper;

    @Resource
    private LotteryResultDetailService lotteryResultDetailService;


    /**
     * 已结束的摇号批次进行结果查看
     * @param page
     * @param batchId
     * @param roundId
     * @return
     */
    public List<LotteryResultDetailBO> viewResult(Page<LotteryResultDetailPO> page, Long batchId, Long roundId) {
        //1.根据摇号批次的id和轮数查询结果id
        LotteryResultPO po = mapper.selectOne(new LambdaQueryWrapper<LotteryResultPO>()
                .eq(LotteryResultPO::getBatchId, batchId)
                .eq(LotteryResultPO::getRoundId, roundId));

        if (null == po){
            return null;
        }

        //2.根据结果id查询对应的结果详情
        List<LotteryResultDetailBO> boList = lotteryResultDetailService.selectDetailListByResultId(page,po.getId());
        return boList;
    }
}
