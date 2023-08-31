package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * @author: lpy
 * @Date: 2023/03/27
 */
@Data
public class BlackListBatchDelReq {
    @ApiModelProperty(value = "批量要删除的车牌号")
    private List<Long> blackListIds;
}
