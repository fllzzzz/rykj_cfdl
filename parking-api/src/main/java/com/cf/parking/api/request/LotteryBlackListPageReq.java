package com.cf.parking.api.request;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号黑名单
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
public class LotteryBlackListPageReq {

    /** id */
    private Long id;

    /** userId */
    private Long userId;

    /** 原因 */
    private String reason;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
