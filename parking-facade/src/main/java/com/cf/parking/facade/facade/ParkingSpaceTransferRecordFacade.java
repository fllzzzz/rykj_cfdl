package com.cf.parking.facade.facade;

/**
 * 车位转赠记录Service接口
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
public interface ParkingSpaceTransferRecordFacade
{

	/**
	 * 车位转让
	 * @param openId 转让人
	 * @param jobNum 受让人
	 */
	void transfer(String outJobNum, String inJobNum);

}
