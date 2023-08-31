package com.cf.parking.facade.enums;

public enum RequestFlagEnum {
	REQUEST_FLAG_NO(1, "未发过"),
	REQUEST_FLAG_SENT(2, "已发过");

	RequestFlagEnum(int code, String msg) {
		this.code = code;
		this.msg = msg;
	}

	private int code;
	private String msg;

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
