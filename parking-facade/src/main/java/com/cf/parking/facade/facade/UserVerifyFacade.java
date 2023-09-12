package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.UserVerifyBO;
import com.cf.parking.facade.dto.UserVerifyDTO;
import com.cf.parking.facade.dto.UserVerifyOptDTO;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;

/**
 * 车辆审核 Service接口
 * @author
 * @date 2023/9/7
 */
public interface UserVerifyFacade {

    /**
     * 查询车辆审核列表
     * @param dto
     * @return
     */
    PageResponse<UserVerifyBO> getUserVerifyList(UserVerifyDTO dto);

    /**
     * 获取摇车辆审核详细信息
     * @param dto
     * @return
     */
    UserVerifyBO getUserVerify(UserVerifyDTO dto);

    /**
     * 新增车辆审核
     * @param dto
     * @return
     */
    Integer add(UserVerifyOptDTO dto);

    /**
     * 审核车辆
     * @param dto
     * @return
     */
    Integer audit(UserVerifyOptDTO dto);

    /**
     * 批量审核车辆
     * @param dto
     * @return
     */
    void batchAudit(UserVerifyOptDTO dto);
}