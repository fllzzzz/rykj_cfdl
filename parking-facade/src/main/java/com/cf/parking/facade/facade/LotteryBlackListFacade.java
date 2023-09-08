package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.LotteryBlackListBO;
import com.cf.parking.facade.dto.LotteryBlackListDTO;
import com.cf.parking.facade.dto.LotteryBlackListOptDTO;
import com.cf.support.result.PageResponse;

import java.util.List;


/**
 * 摇号黑名单Service接口
 * 
 * @author
 * @date 2023-09-05
 */
public interface LotteryBlackListFacade
{

    /**
     * 查询摇号黑名单列表
     * @param dto
     * @return
     */
    PageResponse<LotteryBlackListBO> getLotteryBlackList(LotteryBlackListDTO dto);

    /**
     * 新增摇号黑名单
     * @param dto
     * @return
     */
    Integer add(LotteryBlackListOptDTO dto);

    /**
     * 修改摇号黑名单
     * @param dto
     * @return
     */
    Integer update(LotteryBlackListOptDTO dto);

    /**
     * 移出摇号黑名单
     * @param id
     * @return
     */
    Integer deleteById(Long id);
}
