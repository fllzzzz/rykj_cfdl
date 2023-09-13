package com.cf.parking.facade.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * 停车场园区dto
 * @author
 * @date 2023/9/13
 */
@Data
@Accessors(chain = true)
public class ParkingLotAreaOptDTO {

    /** 园区名称 */
    private String name;

    /** 入口列表 */
    private List<ParkingLotAreaEntranceOptDTO> entranceList;

}
