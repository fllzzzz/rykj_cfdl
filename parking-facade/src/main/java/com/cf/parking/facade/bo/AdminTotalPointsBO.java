package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2022/10/28
 */
@Data
@Accessors(chain = true)
public class AdminTotalPointsBO {
	private Integer totalPoints;

	private Integer redeemedPoints;

	private Integer unchangedPoints;
}
