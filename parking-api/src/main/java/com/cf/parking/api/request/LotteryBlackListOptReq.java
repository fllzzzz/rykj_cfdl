package com.cf.parking.api.request;

import java.util.Date;

/**
 * @author
 * @date 2023/9/6
 */
public class LotteryBlackListOptReq {

    /** id */
    private Long id;

    /** userId */
    private Long userId;

    /** 工号 */
    private String jobNumber;

    /** 姓名 */
    private String name;

    /** 原因 */
    private String reason;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
