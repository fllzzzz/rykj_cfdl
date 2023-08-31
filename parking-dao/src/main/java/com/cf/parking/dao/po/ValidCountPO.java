package com.cf.parking.dao.po;

import lombok.Data;

/**
 * @Classname VaildCountPO
 * @Date 2022/11/15 9:14
 * @Created by csy
 */
@Data
public class ValidCountPO {
    /**
     * 用户id
     */
    private Long userId;
    /**
     * 次数
     */
    private Long count;

    /**
     * 姓名
     */
    private String name;

    /**
     * 工号
     */
    private String jobNumber;

}
