package com.cf.parking.facade.bo;

import java.util.Date;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author think
 * 车位对象，根据车库/有效期分组
 */

@Data
@Accessors(chain = true)
public class ParkingSpaceGroupBO {

	
	/**
     * 结束时间
     */
    private Date endDate;

    /**
     * 工号
     */
    private String jobNumber;

    
    /**
     * 所属车场名
     */
    private String parkingLot;
    
    /**
     * 开始时间
     */
    private Date startDate;

}
