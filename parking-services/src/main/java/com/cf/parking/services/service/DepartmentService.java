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
import com.cf.parking.dao.mapper.DepartmentPOMapper;
import com.cf.parking.dao.po.DepartmentPO;


@Service
public class DepartmentService extends ServiceImpl<DepartmentPOMapper, DepartmentPO> implements IService<DepartmentPO>{

	
	@Resource
	private DepartmentPOMapper departmentPOMapper;

	/**
	 * 查询状态正常的部门号
	 * @param deptCodeList
	 * @return
	 */
	public List<String> queryDeptCodeInUse(List<String> deptCodeList) {
		return CollectionUtils.isEmpty(deptCodeList) ? Collections.EMPTY_LIST : departmentPOMapper.selectList(new LambdaQueryWrapper<DepartmentPO>()
					.in(DepartmentPO::getDeptCode, deptCodeList)
					.eq(DepartmentPO::getState, 0)
				).stream().map(item -> item.getDeptCode()).collect(Collectors.toList());
	}


	/**
	 * 查询状态为正常的部门列表
	 * @return
	 */
	public List<DepartmentPO> queryUsingDepartmentList() {
		List<DepartmentPO> poList = departmentPOMapper.selectList(new LambdaQueryWrapper<DepartmentPO>()
				.eq(DepartmentPO::getState, 0));
		return poList;
	}
}

