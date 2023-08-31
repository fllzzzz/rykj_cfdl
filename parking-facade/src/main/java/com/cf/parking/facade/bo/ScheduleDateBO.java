package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author whx
 * @date 2023/3/28
 */
@Data
@Accessors(chain = true)
public class ScheduleDateBO {

	/**
	 * 排班日期
	 */
	private List<String> shiftDateList;
}
