package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 摇号结果详情
 * @author
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class LotteryResultDetailDTO {

    /** 摇号结果表id */
    private Long resultId;

    /** 状态（0：未同步；1：同步成功；2：同步失败） */
    private String state;

    /** 摇号批次记录id，跟上面几个属性是互斥的 */
    private Long batchId;
}
