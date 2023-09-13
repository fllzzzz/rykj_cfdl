package com.cf.parking.facade.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 停车场园区入口dto
 * @author
 * @date 2023/9/13
 */
@Data
@Accessors(chain = true)
public class ParkingLotAreaEntranceOptDTO {

    //入口名称
    private String region;

    //入口编号
   private String regionCode;

}
