package com.cf.parking.facade.enums;

public enum SourceTypeEnum {
	AUTO_COUNT_INTEGRAL_TASK(1, "月底加积分");
	private Integer code;
	private String name;

	SourceTypeEnum(Integer code, String name) {
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
