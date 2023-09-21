package com.cf.parking.services.enums;




/**
 * 闸机返回的错误码
 */
public enum ParkingRemoteCodeEnum {

	RESP_SUCCESS("0","请求状态码"),
	BUS_CODE("200","业务成功状态码"),
	UNAUTH("405230602","未授权")
	;
	
	private String state;
	
	private String remark;

	private ParkingRemoteCodeEnum(String state, String remark) {
		this.state = state;
		this.remark = remark;
	}

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getRemark() {
		return remark;
	}

	public void setRemark(String remark) {
		this.remark = remark;
	}
	
	
}
