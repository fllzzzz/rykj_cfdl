package com.cf.parking.facade.bo;

import com.fasterxml.jackson.annotation.JsonFormat;

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

    /** 工号 */
    private String jobNumber;

    /** 姓名 */
    private String name;

    /** 原因 */
    private String reason;

    /** 创建时间 */
    @JsonFormat(pattern = "yyyy-MM-dd hh:mm:ss")
    private Date createTm;

    /** 更新时间 */
    @JsonFormat(pattern = "yyyy-MM-dd hh:mm:ss")
    private Date updateTm;
}
