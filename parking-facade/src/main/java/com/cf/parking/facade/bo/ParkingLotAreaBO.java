package com.cf.parking.facade.bo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * 停车场园区
 * @author
 * @date 2023/9/14
 */
@Data
@Accessors(chain = true)
public class ParkingLotAreaBO {

    /** id */
    private Long id;

    /** 园区名称 */
    private String name;

    /** 入口列表 */
    private List<ParkingLotAreaEntranceBO> entranceList;
}
