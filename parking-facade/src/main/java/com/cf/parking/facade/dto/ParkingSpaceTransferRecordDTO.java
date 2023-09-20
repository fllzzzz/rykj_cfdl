package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 车位转赠记录
 * @author
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class ParkingSpaceTransferRecordDTO extends PageRequest {
    /** 转赠日期（起） */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validStartDate;

    /** 转赠日期（止） */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validEndDate;

    /** userId */
    private Long userId;
}
