package com.cf.parking.facade.bo;



import lombok.Data;
import lombok.experimental.Accessors;




/**
 * 闸机返回基类
 * @author think
 * @param <T>
 *
 */
@Data
@Accessors(chain = true)
public class ParkBaseRespBO<T> {

	private String resCode;
	
	private String resMsg;
	
	private T result;
}
