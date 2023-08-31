package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2022/10/21
 */
@Data
@Accessors(chain = true)
public class ScoreRecordBO {
	private Long parkingOrderId;

	private String remark;

	private String createTm;

	private Integer score;

	private String emplNo;

	private String adminName;
}
