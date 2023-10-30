package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.LotteryApplyBO;
import com.cf.parking.facade.bo.LotteryApplyRecordBO;
import com.cf.parking.facade.dto.LotteryApplyRecordDTO;
import com.cf.support.result.PageResponse;


/**
 * 摇号申请记录Service接口
 * 
 * @author
 * @date 2023-09-05
 */
public interface LotteryApplyRecordFacade
{

    /**
     * 查询摇号申请记录列表
     * @param dto
     * @return
     */
    PageResponse<LotteryApplyRecordBO> getApplyRecordList(LotteryApplyRecordDTO dto);

    /**
     * 个人申请摇号页面信息查询
     * @param userId
     * @return
     */
    LotteryApplyBO info(Long userId);

    /**
     * 申请摇号
     * @param userId
     * @param batchId
     * @return
     */
    Integer apply(Long userId, Long batchId);

    /**
     * 取消摇号
     * @param userId
     * @param batchId
     * @return
     */
    Integer cancel(Long userId, Long batchId);

    /**
     * 判断当前时间是否在该摇号批次的报名时间内
     * @param batchId
     * @return
     */
    boolean judgeInApplyTime(Long batchId);
}
