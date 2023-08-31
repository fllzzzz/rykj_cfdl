package com.cf.parking.facade.enums;

/**
 * 通用是否枚举类
 */
public enum BooleanEnum {
	TRUE(1, "是"),
	FALSE(2, "否");

	BooleanEnum(Integer code, String msg) {
		this.code = code;
		this.msg = msg;
	}

	private Integer code;
	private String msg;

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
