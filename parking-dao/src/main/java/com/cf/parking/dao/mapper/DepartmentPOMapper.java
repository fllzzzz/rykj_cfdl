package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.DepartmentPO;

import java.util.List;


public interface DepartmentPOMapper extends BaseMapper<DepartmentPO> {
    int deleteByPrimaryKey(String deptCode);

    int insert(DepartmentPO record);

    int insertSelective(DepartmentPO record);

    DepartmentPO selectByPrimaryKey(String deptCode);

    int updateByPrimaryKeySelective(DepartmentPO record);

    int updateByPrimaryKey(DepartmentPO record);

    /**
     * 查询部门列表中的顶级节点
     * @return
     */
    List<DepartmentPO> getTopParentList();
}