package com.cf.parking.facade.enums;

/**
 * 通用是否枚举类
 */
public enum IsDeleteEnum {
	TRUE(1, "已删除"),
	FALSE(0, "未删除");

	IsDeleteEnum(Integer code, String msg) {
		this.code = code;
		this.msg = msg;
	}

	static {

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
