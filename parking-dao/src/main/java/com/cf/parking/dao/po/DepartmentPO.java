package com.cf.parking.dao.po;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;


import java.util.Date;

/**
 * @author 
 * @date 2023-09-08 13:53:13
 *
 * @description 部门表
 */
@Data
@TableName("department")
@Accessors(chain = true)
public class DepartmentPO  {


    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 部门名称
     */
    private String departmentName;

    /**
     * 部门编码
     */
    @TableId(value = "dept_code", type =  IdType.INPUT )
    private String deptCode;

    /**
     * 部门全路径
     */
    private String fullPath;

    /**
     * 部门状态，0为正常，1为失效
     */
    private Integer state;

    /**
     * 父级部门code
     */
    private String parentCode;
}
