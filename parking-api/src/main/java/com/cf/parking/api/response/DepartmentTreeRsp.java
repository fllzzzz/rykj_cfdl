package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author
 * @date 2023/9/13
 */
@Data
@ApiModel(description = "部门树状结构")
@Accessors(chain = true)
public class DepartmentTreeRsp {

    //部门编码
    @ApiModelProperty(value = "部门编码")
    private String code;

    //部门名称
    @ApiModelProperty(value = "部门名称")
    private String name;

    //子部门信息
    @ApiModelProperty(value = "子部门信息")
    private List<DepartmentTreeRsp> children;
}
