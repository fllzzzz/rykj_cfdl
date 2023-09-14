package com.cf.parking.api.response;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author
 * @date 2023/9/13
 */
@Data
@ApiModel(description = "部门列表")
@Accessors(chain = true)
public class DepartmentBaseRsp {

    //部门编码
    @ApiModelProperty(value = "部门编码")
    private String deptCode;

    //部门名称
    @ApiModelProperty(value = "部门名称")
    private String departmentName;
}
