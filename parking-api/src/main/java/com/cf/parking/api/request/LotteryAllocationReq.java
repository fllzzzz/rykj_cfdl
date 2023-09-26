package com.cf.parking.api.request;

import lombok.Data;



/**
 * 分配停车场入参
 * @author think
 *
 */
@Data
public class LotteryAllocationReq {
	
	/**
	 * 批次id
	 */
	private Long id;
	
	/**
	 * 批次id
	 */
	private String parkingCode;
}
