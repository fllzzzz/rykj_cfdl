package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author whx
 * @date 2023/4/3
 */
@Data
@Accessors(chain = true)
public class DingNoticeRecordDTO implements Serializable {
	/**
	 * 工号
	 */
	private String jobNumber;

	/**
	 * 通知内容
	 */
	private String message;
}
