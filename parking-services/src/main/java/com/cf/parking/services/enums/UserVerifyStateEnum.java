package com.cf.parking.services.enums;


import io.swagger.models.auth.In;

/**
 * 车辆审核状态
 * @author think
 */
public enum UserVerifyStateEnum {

	UNAUDIT(1,"待审核"),
	FAILED(2,"审核不通过"),
	SUCCESS(3,"审核通过"),
	
	;
	
	
	private Integer state;
	
	private String remark;

	private UserVerifyStateEnum(Integer state, String remark) {
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
