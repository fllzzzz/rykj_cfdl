package com.cf.parking.services.enums;




/**
 * 启用/停用状态
 * @author think
 */
public enum EnableStateEnum {

	ENABLE("1","启用"),
	DISABLE("0","启用");
	
	private String state;
	
	private String remark;

	private EnableStateEnum(String state, String remark) {
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
