package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号黑名单
 * @author
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class LotteryBlackListDTO {
    /** id */
    private Long id;

    /** 工号 */
    private String jobNumber;

    /** 姓名 */
    private String name;

}
