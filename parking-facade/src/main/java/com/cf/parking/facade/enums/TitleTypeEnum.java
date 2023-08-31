package com.cf.parking.facade.enums;

public enum TitleTypeEnum {
	DRIVER_TITLE(1, "车主"),
	PASSENGER_TITLE(2, "乘客"),
	;
	private int code;
	private String msg;

	TitleTypeEnum(int code, String msg) {
		this.code = code;
		this.msg = msg;
	}

	public int getCode() {
		return code;
	}

	public void setCode(int code) {
		this.code = code;
	}

	public String getMsg() {
		return msg;
	}

	public void setMsg(String msg) {
		this.msg = msg;
	}
}
