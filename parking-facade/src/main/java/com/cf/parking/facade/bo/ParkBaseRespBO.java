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
	
	
	public static ParkBaseRespBO fail() {
		return new ParkBaseRespBO().setResCode("500").setResMsg("调用异常");
	}
	
	public static ParkBaseRespBO fail(String msg) {
		return new ParkBaseRespBO().setResCode("500").setResMsg(msg);
	}
}
