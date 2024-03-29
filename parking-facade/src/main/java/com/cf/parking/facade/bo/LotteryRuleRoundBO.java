package com.cf.parking.facade.bo;

import lombok.Data;

import java.util.Date;

/**
 * 摇号规则-轮数
 * @author
 * @date 2023/9/5
 */
@Data
public class LotteryRuleRoundBO {
    /** id */
    private Long id;

    /** 轮数名称 */
    private String name;

    /** 停车场(编号)，多个间逗号间隔 */
    private String parkingLotCode;

    /** 停车场(编号)，多个间逗号间隔 */
    private String parkingLotName;

    /** 停车场数量 */
    private Long parkingLotAmount;

    /** 状态(0：停用，1：启用) */
    private String state;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;

    /** 备注 */
    private String remark;
}
