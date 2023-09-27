package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * 部门树状结构
 * @author
 * @date 2023/9/27
 */
@Data
@Accessors(chain = true)
public class DepartmentTreeBO {

    //部门编码
    private String code;

    //部门名称
    private String name;

    //父级部门code
    private String parentCode;

    //子部门信息
    private List<DepartmentTreeBO> children;
}
