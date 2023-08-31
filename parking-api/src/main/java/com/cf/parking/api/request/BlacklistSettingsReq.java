package com.cf.parking.api.request;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

/**
 * @author lpy
 * @date 2023-03-27 10:29:20
 * @description 黑名单设置表
 */
@Data
@Accessors(chain = true)
public class BlacklistSettingsReq {
    @ApiModelProperty(value = "id")
    private Long blacklistSettingsId;

    @ApiModelProperty(value = "累计次数")
    @Max(value = 10, message = "累计次数大小不合法")
    @Min(value = 0, message = "累计次数大小不合法")
    @NotNull(message = "累计次数不能为空")
    private Integer cumulativeNumber;

    @ApiModelProperty(value = "无出入记录时长")
    @Max(value = 100, message = "无出入记录时长不合法")
    @Min(value = 0, message = "无出入记录时长不合法")
    @NotNull(message = "无出入记录时长不能为空")
    private Integer noRecordInTime;

}
