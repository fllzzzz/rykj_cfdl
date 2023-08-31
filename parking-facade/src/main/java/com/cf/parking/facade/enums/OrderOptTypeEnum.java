package com.cf.parking.facade.enums;

public enum OrderOptTypeEnum {
	CONFIRM_BOARDING(1, "确认上车"),
	ARRIVE_BY_PASSENGER(2, "到达目的地");
	private Integer code;
	private String msg;

	OrderOptTypeEnum(Integer code, String msg) {
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
