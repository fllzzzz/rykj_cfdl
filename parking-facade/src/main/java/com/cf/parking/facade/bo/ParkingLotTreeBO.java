package com.cf.parking.facade.bo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * @author
 * @date 2023/9/15
 */
@Data
public class ParkingLotTreeBO {

    /** id */
    private Long id;

    /** 停车场区域 */
    private String region;

    /** 停车场区域编号 */
    private String regionCode;

    /** children */
    @ApiModelProperty(value = "子记录")
    private List<ParkingLotTreeBO> children;

}
