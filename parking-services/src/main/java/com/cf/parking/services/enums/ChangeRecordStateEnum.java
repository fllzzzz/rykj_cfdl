package com.cf.parking.services.enums;




/**
 * 车位交换表状态
 * @author think
 */
public enum ChangeRecordStateEnum {

	APPLY("0","申请"),
	AGREE("1","已同意"),
	REJECT("2","已拒绝"),
	CANCEL("3","已撤销"),
	
	;
	
	
	private String state;
	
	private String remark;

	private ChangeRecordStateEnum(String state, String remark) {
		this.state = state;
		this.remark = remark;
	}
	
	public static ChangeRecordStateEnum getStateEnum(String state) {
		for (ChangeRecordStateEnum value : values()) {
			if (value.getState().equals(state)) {
				return value;
			}
		}
		return null;
	}

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getRemark() {
		return remark;
	}

	public void setRemark(String remark) {
		this.remark = remark;
	}
	
	
}
