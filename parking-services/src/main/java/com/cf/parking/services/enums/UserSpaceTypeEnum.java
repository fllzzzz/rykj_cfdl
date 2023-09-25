package com.cf.parking.services.enums;




/**
 * 车位表同步闸机状态
 * @author think
 */
public enum UserSpaceTypeEnum {

	LOTTERY(1,"摇号"),
	SETTING(2,"默认分配"),
	
	;
	
	
	private Integer state;
	
	private String remark;

	private UserSpaceTypeEnum(Integer state, String remark) {
		this.state = state;
		this.remark = remark;
	}

	public Integer getState() {
		return state;
	}

	public void setState(Integer state) {
		this.state = state;
	}

	public String getRemark() {
		return remark;
	}

	public void setRemark(String remark) {
		this.remark = remark;
	}
	
	
}
