package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Data
public class OrderRecordReq extends PageRequest {
    @ApiModelProperty(value = "用户类型 (1:搭车,2:开车)")
    private Integer userType;
}
