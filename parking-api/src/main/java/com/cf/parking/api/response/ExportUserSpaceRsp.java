package com.cf.parking.api.response;

import cn.afterturn.easypoi.excel.annotation.Excel;
import lombok.Data;

/**
 * @author whx
 * @date 2023/2/10
 */
@Data
public class ExportUserSpaceRsp {

	/**
	 * 所属车场名(多个逗号分割)
	 */
	@Excel(name = "所属车场", orderNum = "1")
	private String parkingLot;

	@Excel(name = "工号", orderNum = "2")
	private String jobNumber;

	@Excel(name = "姓名", orderNum = "3")
	private String name;

	@Excel(name = "车牌号", orderNum = "4")
	private String plateNo;

	@Excel(name = "有效起始日期", orderNum = "5")
	private String startDate;

	@Excel(name = "有效截止日期", orderNum = "6")
	private String endDate;

}
