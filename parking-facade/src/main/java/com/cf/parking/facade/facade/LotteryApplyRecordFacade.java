package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.LotteryApplyRecordBO;
import com.cf.parking.facade.dto.LotteryApplyRecordDTO;
import com.cf.support.result.PageResponse;

import java.util.List;

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

}
