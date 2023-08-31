package com.cf.parking.facade.enums;

public enum OrderDateEnum {
	TODAY(1, "今天"),
	TOMORROW(2, "明天"),
	THE_DAY_AFTER_TOMORROW(3, "后天");
	private Integer code;
	private String msg;

	OrderDateEnum(Integer code, String msg) {
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
