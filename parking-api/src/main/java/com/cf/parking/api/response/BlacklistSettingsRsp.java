package com.cf.parking.api.response;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author lpy
 * @date 2023-03-27 10:29:20
 * @description 黑名单设置表
 */
@Data
@Accessors(chain = true)
public class BlacklistSettingsRsp {
    @ApiModelProperty(value = "id")
    private Long blacklistSettingsId;

    @ApiModelProperty(value = "创建时间")
    private Date createTm;

    @ApiModelProperty(value = "累计次数")
    private Integer cumulativeNumber;

    @ApiModelProperty(value = "无出入记录时长")
    private Integer noRecordInTime;

}
