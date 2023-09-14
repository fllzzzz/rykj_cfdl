package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.ParkingSpaceTransferRecordBO;
import com.cf.parking.facade.dto.ParkingSpaceTransferRecordDTO;
import com.cf.support.result.PageResponse;

/**
 * 车位转赠记录Service接口
 * 
 * @author
 * @date 2023-09-05
 */
public interface ParkingSpaceTransferRecordFacade
{

	/**
	 * 车位转让
	 * @param outJobNum 转让人
	 * @param inJobNum 受让人
	 */
	void transfer(String outJobNum, String inJobNum);

    /**
     * 查询车位转赠记录列表
     * @param dto
     * @return
     */
    PageResponse<ParkingSpaceTransferRecordBO> getParkingSpaceTransferRecordList(ParkingSpaceTransferRecordDTO dto);
}
