package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

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
