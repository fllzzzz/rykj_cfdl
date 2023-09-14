package com.cf.parking.api.response;

import com.cf.parking.api.request.ParkingLotAreaEntranceOptReq;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * @author
 * @date 2023/9/14
 */
@Data
@ApiModel(description = "停车场园区结果")
public class ParkingLotAreaRsp {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** 园区名称 */
    @ApiModelProperty(value = "园区名称")
    private String name;

    /** 入口列表 */
    @ApiModelProperty(value = "入口列表")
    private List<ParkingLotAreaEntranceOptReq> entranceList;
}
