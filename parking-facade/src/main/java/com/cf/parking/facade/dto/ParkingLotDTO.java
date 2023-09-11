package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 停车场
 * @author
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class ParkingLotDTO  extends PageRequest {
    /** id */
    private Long id;

    /** 区域 */
    private String region;

    /** 区域编号 */
    private String regionCode;

    /** 类型(0：不可摇号，1：可摇号) */
    private String type;
}
