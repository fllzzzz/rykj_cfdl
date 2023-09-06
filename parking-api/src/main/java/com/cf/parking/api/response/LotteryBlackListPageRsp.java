package com.cf.parking.api.response;

import lombok.Data;

import java.util.Date;

/**
 * 摇号黑名单
 * @author
 * @date 2023/09/05
 */
@Data
public class LotteryBlackListPageRsp {

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
