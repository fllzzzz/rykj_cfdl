package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.ParkingSpaceChangeRecordBO;
import com.cf.parking.facade.dto.ParkingSpaceChangeRecordDTO;
import com.cf.support.result.PageResponse;

/**
 * @author think
 *	车位互换
 */
public interface ParkingSpaceChangeRecordFacade {

	/**
	 * 根据申请人和交换人查询互换记录
	 * @param dto
	 * @return
	 */
	PageResponse<ParkingSpaceChangeRecordBO> getParkingSpaceChangeRecordList(ParkingSpaceChangeRecordDTO dto);

}
