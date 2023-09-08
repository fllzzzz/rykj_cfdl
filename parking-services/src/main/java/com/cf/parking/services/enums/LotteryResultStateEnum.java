package com.cf.parking.services.enums;




/**
 * 启用/停用状态
 * @author think
 */
public enum LotteryResultStateEnum {

	UNLOTTERY("0","带摇号"),
	UNCONFIRM("1","待确认"),
	CONFIRM_IN_PROCESS("2","确认中"),
	UNPUBLIC("3","待发布"),
	UNARCHIVED("4","待归档"),
	
	;
	
	
	private String state;
	
	private String remark;

	private LotteryResultStateEnum(String state, String remark) {
		this.state = state;
		this.remark = remark;
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
