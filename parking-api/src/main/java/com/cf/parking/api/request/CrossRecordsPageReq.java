package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author: lpy
 * @Date: 2023/03/28
 */
@Data
public class CrossRecordsPageReq extends PageRequest {

    @ApiModelProperty(value = "车牌号")
    private String plateNo;
}
