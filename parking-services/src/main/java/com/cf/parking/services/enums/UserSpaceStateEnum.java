package com.cf.parking.services.enums;




/**
 * 车位表同步闸机状态
 * @author think
 */
public enum UserSpaceStateEnum {

	UNSYNC("0","待同步"),
	SUCCESS("1","同步成功"),
	FAIL("2","同步失败"),
	
	;
	
	
	private String state;
	
	private String remark;

	private UserSpaceStateEnum(String state, String remark) {
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
