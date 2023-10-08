package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;


/**
 * 摇号黑名单
 * @author
 * @date 2023/9/8
 */
@Data
@Accessors(chain = true)
public class LotteryBlackListOptDTO  {
    /** id */
    private Long id;

    /** 工号 */
    private String code;

    /** 姓名 */
    private String name;

    /** 原因 */
    private String reason;

}
