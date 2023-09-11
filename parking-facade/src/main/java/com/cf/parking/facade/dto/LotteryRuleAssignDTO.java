package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号规则-停车场分配
 * @author
 * @date 2023/9/5
 */
@Data
@Accessors(chain = true)
public class LotteryRuleAssignDTO extends PageRequest {

    /** id */
    private Long id;

    /** 分配类型（1：按部门分配；2：按人员分配） */
    private String type;

    /** 名称（部门或者人员名称） */
    private String name;

    /** 停车场(编号)，多个间逗号间隔 */
    private String parkingLotCode;

    /** 状态(0：停用，1：启用) */
    private String state;
}
