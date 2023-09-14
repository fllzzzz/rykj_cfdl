package com.cf.parking.facade.bo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 停车场园区入口
 * @author
 * @date 2023/9/14
 */
@Data
@Accessors(chain = true)
public class ParkingLotAreaEntranceBO {
    //入口名称
    private String region;

    //入口编号
    private String regionCode;

}
