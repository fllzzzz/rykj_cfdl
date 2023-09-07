package com.cf.parking.api.request;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号结果详情
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
@ApiModel(description = "摇号结果详情查询对象")
public class LotteryResultDetailReq {

    /** 摇号结果表id */
    @ApiModelProperty(value = "摇号结果表id")
    private Long resultId;

    /** 状态（0：未同步；1：同步成功；2：同步失败） */
    @ApiModelProperty(value = "状态")
    private String state;

    /** 摇号批次记录id，跟上面几个属性是互斥的 */
    @ApiModelProperty(value = "摇号批次记录id，在摇号批次界面点击查看结果时使用")
    private Long batchId;

}
