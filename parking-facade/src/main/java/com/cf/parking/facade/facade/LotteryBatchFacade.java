package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.LotteryBatchBO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.bo.LotteryResultExportBO;
import com.cf.parking.facade.dto.LotteryBatchDTO;
import com.cf.parking.facade.dto.LotteryBatchOptDTO;
import com.cf.support.result.PageResponse;

import java.util.Date;
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

    /**
     * 判断本期车位有效期是否正确（本期车位有效开始日期要晚于上一批车位有效截止日期）
     * @param validStartDate
     * @return
     */
    boolean judgeValidStartDateUsable(Date validStartDate);


	/**
	 * 分配停车场
	 * @param id 批次ID
	 * @param parkingCode 停车场code
	 */
	void allocationPark(Long id, String parkingCode);

    /**
     * 查询摇号结果导出对象
     * @param batchId
     * @return
     */
    List<LotteryResultExportBO> exportResult(Long batchId);


    LotteryBatchBO getLatestBatchInfo();
}
