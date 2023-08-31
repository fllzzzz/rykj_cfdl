package com.cf.parking.api.response;

import cn.afterturn.easypoi.excel.annotation.Excel;
import lombok.Data;

/**
 * @author whx
 * @date 2022/10/22
 */
@Data
public class ExportScoreListRsp {
	@Excel(name = "姓名", orderNum = "1")
	private String name;

	@Excel(name = "工号", orderNum = "2")
	private String jobNumber;

	@Excel(name = "总分", orderNum = "3")
	private Integer totalScore;
}
