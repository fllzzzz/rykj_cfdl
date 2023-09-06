package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.LotteryBatchBO;
import com.cf.parking.facade.dto.LotteryBatchDTO;
import com.cf.support.result.PageResponse;

import java.util.List;


/**
 * 摇号批次Service接口
 * 
 * @author
 * @date 2023-09-05
 */
public interface LotteryBatchFacade
{

    /**
     * 查询摇号批次列表
     * @param dto
     * @return
     */
    PageResponse<LotteryBatchBO> getLotteryBatchList(LotteryBatchDTO dto);
}
