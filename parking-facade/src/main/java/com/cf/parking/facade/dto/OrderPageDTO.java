package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2022/10/19
 */
@Data
@Accessors(chain = true)
public class OrderPageDTO extends PageRequest {
	private String startAddress;

	private String destAddress;
}