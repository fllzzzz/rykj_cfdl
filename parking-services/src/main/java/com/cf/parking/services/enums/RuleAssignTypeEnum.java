package com.cf.parking.services.enums;




/**
 * lottery_rule_assign 表的type枚举
 * @author think
 */
public enum RuleAssignTypeEnum {

	DEPARMENT("1","部门"),
	EMPLOYEE("2","人员");
	
	private String state;
	
	private String remark;

	private RuleAssignTypeEnum(String state, String remark) {
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
