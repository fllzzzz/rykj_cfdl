package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.UserSpaceBO;
import com.cf.parking.facade.dto.UserSpacePageDTO;
import com.cf.support.result.PageResponse;

import java.util.List;

/**
 * @author lpy
 * @date 2023/2/9
 */
public interface UserSpaceFacade {

    /**
     * 车位分页查询
     *
     * @param param
     * @return
     */
    PageResponse<UserSpaceBO> getUserSpacePage(UserSpacePageDTO param);

    /**
     * 车位列表查询
     *
     * @param param
     * @return
     */
    List<UserSpaceBO> getUserSpaceList(UserSpacePageDTO param);

    /**
     * 同步海康查询车位信息（查询车辆包期）
     *
     * @throws InterruptedException
     */
    void syncUserSpaceData() throws InterruptedException;
}
