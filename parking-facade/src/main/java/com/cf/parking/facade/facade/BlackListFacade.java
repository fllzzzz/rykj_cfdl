package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.BlackListBO;
import com.cf.parking.facade.bo.BlackListPageBO;
import com.cf.parking.facade.bo.BlacklistSettingsBO;
import com.cf.parking.facade.dto.BlackListBatchAdditionDTO;
import com.cf.parking.facade.dto.BlackListBatchDelDTO;
import com.cf.parking.facade.dto.BlackListPageDTO;
import com.cf.parking.facade.dto.BlacklistSettingsDTO;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;

import java.util.List;

/**
 * @author: lpy
 * @Date: 2023/03/27
 */
public interface BlackListFacade {
    /**
     * 黑名单导出
     *
     * @return list<BlackListBO>
     */
    List<BlackListBO> export();

    /**
     * 批量删除黑名单
     *
     * @param param plateNos
     * @return Result
     */
    Result blackListBatchDel(BlackListBatchDelDTO param);

    /**
     * 批量添加黑名单
     *
     * @param list 添加list
     * @return Result
     */
    Result blackListAddition(List<BlackListBatchAdditionDTO> list);

    /**
     * 批量查询黑名单
     *
     * @param param param
     * @return pageResponse
     */
    PageResponse<BlackListPageBO> page(BlackListPageDTO param);

    /**
     * 设置保存接口
     *
     * @param param param
     * @return Result
     */
    Result settingsSave(BlacklistSettingsDTO param);

    /**
     * 设置查询接口
     *
     * @return Result
     */
    BlacklistSettingsBO settingsGet();
}
