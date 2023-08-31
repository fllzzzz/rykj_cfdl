package com.cf.parking.facade.enums;

public enum EvaluateTypeEnum {
	EVALUATE_DRIVER(1, "评价司机"),
	EVALUATE_PASSENGER(2, "评价乘客");
	private Integer code;
	private String name;

	EvaluateTypeEnum(Integer code, String name) {
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
