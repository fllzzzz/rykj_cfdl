package com.cf.parking.dao.mapper;

import java.util.List;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.EmployeePO;



public interface EmployeePOMapper extends BaseMapper<EmployeePO> {

	List<EmployeePO> selectEmployeeList(String name);



}