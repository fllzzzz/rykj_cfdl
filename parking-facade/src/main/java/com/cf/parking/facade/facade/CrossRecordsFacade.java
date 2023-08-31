package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.CrossRecordsBO;
import com.cf.support.result.PageResponse;

/**
 * @author: lpy
 * @Date: 2023/03/28
 */
public interface CrossRecordsFacade {
    /**
     * 获取过车记录接口分页列表
     *
     * @param pageNo   pageNo
     * @param pageSize pageSize
     * @param plateNo  车牌号
     * @return
     */
    PageResponse<CrossRecordsBO> getPage(Long pageNo, Long pageSize, String plateNo);

    /**
     * 定时任务获取过车记录
     */
    Integer saveCrossRecords(Integer pageNo);
}
