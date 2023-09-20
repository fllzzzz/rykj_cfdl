package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.UserVerifyBO;
import com.cf.parking.facade.dto.UserVerifyDTO;
import com.cf.parking.facade.dto.UserVerifyOptDTO;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;

import java.util.List;

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

    /**
     * 根据userId获取个人车牌号列表
     * @param userId
     * @return
     */
    List<String> getPlatNoListByUserId(Long userId);

    /**
     * 修改审核车辆
     * @param dto
     * @return
     */
    Integer update(UserVerifyOptDTO dto);

    /**
     * 删除个人车辆信息
     * @param id
     * @return
     */
    Integer deleteById(Long id);
}
