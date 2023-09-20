package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.LotteryBatchBO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.dto.LotteryBatchDTO;
import com.cf.parking.facade.dto.LotteryBatchOptDTO;
import com.cf.support.result.PageResponse;



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


    /**
     * 新增摇号批次
     * @param dto
     * @return
     */
    Integer add(LotteryBatchOptDTO dto);

    /**
     * 修改摇号批次
     * @param dto
     * @return
     */
    Integer update(LotteryBatchOptDTO dto);

    /**
     * 删除摇号批次
     * @param id
     * @return
     */
    Integer deleteById(Long id);

    /**
     * 已结束的摇号批次进行结果查看
     * @param dto
     * @return
     */
    PageResponse<LotteryResultDetailBO> viewResult(LotteryBatchDTO dto);

    /**
     * 根据摇号轮数查询车位数量
     * @param roundIdArr
     * @return
     */
    Long getParkingAmountByRound(Long[] roundIdArr);


    /**
     * 钉钉通知所有用户摇号批次信息
     * @param id
     */
    Integer notifyAllUserByBatchId(Long id);
}
