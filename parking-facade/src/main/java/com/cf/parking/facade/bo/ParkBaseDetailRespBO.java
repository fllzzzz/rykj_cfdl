package com.cf.parking.facade.bo;



import lombok.Data;
import lombok.experimental.Accessors;




/**
 * 闸机返回明细数据
 * @author think
 * @param <T>
 *
 */
@Data
@Accessors(chain = true)
public class ParkBaseDetailRespBO {

	private String code;
	
	private String message;
	
}
