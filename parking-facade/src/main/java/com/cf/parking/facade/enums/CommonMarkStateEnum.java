package com.cf.parking.facade.enums;

/**
 * @author lipengyun
 */

public enum CommonMarkStateEnum {
	NOT_DEAL(0, "未处理"),
	ALREADY_DEAL(1, "已处理");
	private Integer code;
	private String name;

	CommonMarkStateEnum(Integer code, String name) {
		this.code = code;
		this.name = name;
	}

	public Integer getCode() {
		return code;
	}

	public void setCode(Integer code) {
		this.code = code;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
