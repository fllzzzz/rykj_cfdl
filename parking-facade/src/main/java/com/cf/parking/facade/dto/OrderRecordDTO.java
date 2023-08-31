package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import lombok.Data;

/**
 * @author: lpy
 * @Date: 2022/10/22
 */
@Data
public class OrderRecordDTO extends PageRequest {
    private Long userId;

    private Integer userType;
}
