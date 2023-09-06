package com.cf.parking.facade.bo;

import java.util.Date;

/**
 * 摇号黑名单
 * @author
 * @date 2023/9/5
 */
public class LotteryBlackListBO {
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
