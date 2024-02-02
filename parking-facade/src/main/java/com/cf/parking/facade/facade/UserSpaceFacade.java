package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.ParkingSpaceGroupBO;
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

	/**
	 * 根据车库/有效期进行分组
	 * @param jobNumber 工号
	 * @param type 类型 
	 * @return
	 */
	List<ParkingSpaceGroupBO> getUserSpaceGroupByParkingLot(String jobNumber, Integer type);

	/**
	 * 根据车位Id删除车位
	 * @param userSpaceId
	 */
	void deleteUserSpace(Long userSpaceId);
}
