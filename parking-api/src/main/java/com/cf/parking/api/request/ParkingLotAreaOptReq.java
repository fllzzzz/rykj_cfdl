package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * 停车场新增/修改园区的操作对象
 * @author
 * @date 2023/9/13
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "停车场园区操作对象（新增/修改）")
public class ParkingLotAreaOptReq {

    /** 园区名称 */
    @ApiModelProperty(value = "园区名称")
    private String name;

    /** 入口列表 */
    @ApiModelProperty(value = "入口列表")
    private List<ParkingLotAreaEntranceOptReq> entranceList;

}
