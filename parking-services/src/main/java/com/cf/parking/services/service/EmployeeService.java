package com.cf.parking.services.service;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.EmployeePOMapper;
import com.cf.parking.dao.po.EmployeePO;



@Service
public class EmployeeService extends ServiceImpl<EmployeePOMapper, EmployeePO> implements IService<EmployeePO>{

	
	@Resource
	private EmployeePOMapper employeePOMapper;
	
	@Resource
	private DepartmentService departmentService;
	
	
	/**
	 * 根据工号判断是否在职，
	 * @param jobNumberList
	 * @return 在职人员工号
	 */
	public List<String> queryEmployeeListByJobNum(List<String> jobNumberList) {
		return CollectionUtils.isEmpty(jobNumberList) ? Collections.EMPTY_LIST : employeePOMapper.selectList(new LambdaQueryWrapper<EmployeePO>()
					.in(EmployeePO::getEmplNo, jobNumberList)
					.eq(EmployeePO::getState, 0)
				).stream().map(item -> item.getEmplNo()).collect(Collectors.toList());
	}


	/**
	 * 根据人员工号查询部门id
	 * @param jobNumberList
	 * @return
	 */
	public List<String> queryDeptListByJobNum(List<String> jobNumberList) {
		
		List<String> deptCodeList = CollectionUtils.isEmpty(jobNumberList) ? Collections.EMPTY_LIST :  employeePOMapper.selectList(new LambdaQueryWrapper<EmployeePO>()
				.in(EmployeePO::getEmplNo, jobNumberList)
				.eq(EmployeePO::getState, 0)
			).stream().map(item -> item.getDeptCode()).collect(Collectors.toList());
		if (CollectionUtils.isEmpty(deptCodeList)) {
			return Collections.EMPTY_LIST;
		}
		return departmentService.queryDeptCodeInUse(deptCodeList);
	}


	/**
	 * 根据部门查找所属人员工号
	 * @param deptList
	 * @return
	 */
	public List<String> queryEmployeeListByDept(List<String> deptList) {
		return CollectionUtils.isEmpty(deptList) ? Collections.EMPTY_LIST :  employeePOMapper.selectList(new LambdaQueryWrapper<EmployeePO>()
				.in(EmployeePO::getDeptCode, deptList)
				.eq(EmployeePO::getState, 0)
			).stream().map(item -> item.getEmplNo()).collect(Collectors.toList());
	}


	
}
