package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.cf.parking.services.enums.LotteryBatchStateEnum;
import org.springframework.stereotype.Service;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryBatchMapper;
import com.cf.parking.dao.po.LotteryBatchPO;

import javax.annotation.Resource;
import java.util.Date;


@Service
public class LotteryBatchService extends ServiceImpl<LotteryBatchMapper, LotteryBatchPO> implements IService<LotteryBatchPO>{

    @Resource
    private LotteryBatchMapper lotteryBatchMapper;

    /**
     * 将对应批次状态修改为已结束
     * @param batchId
     */
    public Integer endByBatchId(Long batchId) {
        LotteryBatchPO po = new LotteryBatchPO();
        po.setId(batchId);
        po.setState(LotteryBatchStateEnum.HAVE_END.getState());
        return lotteryBatchMapper.updateById(po);
    }

    /**
     * 查询最新一期摇号批次信息（期号最大的、状态为已通知或已结束的）
     * @return
     */
    public LotteryBatchPO getNotifiedLatestBatchInfo() {
        return lotteryBatchMapper.selectOne(new LambdaQueryWrapper<LotteryBatchPO>()
                .ne(LotteryBatchPO::getState, LotteryBatchStateEnum.NEED_NOTIFY.getState())
                .orderByDesc(LotteryBatchPO::getBatchNum));
    }


    /**
     * 判断当前时间是否处于报名时间内
     * @param applyStartTime
     * @param applyEndTime
     * @return
     */
    public boolean judgeWhetherInApplyTime(Date applyStartTime, Date applyEndTime) {
        Date now = new Date();
        return now.before(applyEndTime) && now.after(applyStartTime);
    }
}
