package com.cf.parking.api.request;

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
public class LotteryResultDetailPageReq {

    /** id */
    private Long id;

    /** 摇号结果表id */
    private Long resultId;

    /** 轮数 */
    private Long roundId;

    /** 停车场 */
    private Long parkingLotId;

    /** 用户 */
    private Long userId;

    /** 车牌号 */
    private String plateNo;

    /** 状态（0：未同步；1：同步成功；2：同步失败） */
    private String state;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
