package com.cf.parking.facade.enums;

public enum CancelTypeEnum {
	CANCEL_BY_PASSENGER(1, "乘客取消"),
	CANCEL_BY_DRIVER(2, "司机取消");
	private Integer code;
	private String msg;

	CancelTypeEnum(Integer code, String msg) {
		this.code = code;
		this.msg = msg;
	}

	public Integer getCode() {
		return code;
	}

	public void setCode(Integer code) {
		this.code = code;
	}

	public String getMsg() {
		return msg;
	}

	public void setMsg(String msg) {
		this.msg = msg;
	}
}
