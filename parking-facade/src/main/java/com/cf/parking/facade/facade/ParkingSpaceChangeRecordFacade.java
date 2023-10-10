package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.ParkingSpaceChangeRecordBO;
import com.cf.parking.facade.dto.ParkingSpaceChangeApplyDTO;
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

	/**
	 * 发起互换申请
	 * @param dto
	 */
	void applyChange(ParkingSpaceChangeApplyDTO dto);

	/**
	 * 位交换撤销/同意/否决
	 * @param param
	 */
	void deal(ParkingSpaceChangeApplyDTO param);

	/**
	 * 查询交换数量
	 * @param dto
	 * @return
	 */
	long getParkingSpaceChangeRecoudCount(ParkingSpaceChangeRecordDTO dto);

}
