package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author: lpy
 * @Date: 2023/03/30
 */
@Data
@Accessors(chain = true)
public class UserSpaceFuncTimeDTO {

    private String startTime;

    private String endTime;
}
