package com.cf.parking.services.service;


import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import com.cf.parking.facade.bo.DepartmentTreeBO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.DepartmentPOMapper;
import com.cf.parking.dao.po.DepartmentPO;
import org.springframework.util.StringUtils;


@Slf4j
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
	 * 部门树状结构
	 * (状态为正常0、有上级单位编码的)
	 * @return
	 */
	public List<DepartmentTreeBO> departmentTree() {
		List<DepartmentTreeBO> resultBOList = new ArrayList<>();

		List<DepartmentPO> poList = departmentPOMapper.selectList(new LambdaQueryWrapper<DepartmentPO>()
				.eq(DepartmentPO::getState, 0));
		log.info("查询部门数据:{}",JSON.toJSONString(poList));
		if (!CollectionUtils.isEmpty(poList)){
			List<DepartmentTreeBO> departmentTreeBOList = poList.stream().map(po -> new DepartmentTreeBO().setCode(po.getDeptCode()).setName(po.getDepartmentName()).setParentCode(po.getParentCode())).collect(Collectors.toList());
			//1.查出顶级 107为根目录的上级code代码
			resultBOList = departmentTreeBOList.stream().filter(bo -> "107".equals(bo.getParentCode())).collect(Collectors.toList());
			log.info("获取到部门根节点:{}",JSON.toJSONString(resultBOList));
			//2.递归设置children
			for (DepartmentTreeBO departmentTreeBO : resultBOList) {
				departmentTreeBO.setChildren(getDepartmentChildren(departmentTreeBOList,departmentTreeBO));
			}
		}
		return resultBOList;
	}
	

	private List<DepartmentTreeBO> getDepartmentChildren(List<DepartmentTreeBO> departmentTreeBOList, DepartmentTreeBO departmentTreeBO) {
		List<DepartmentTreeBO> children = departmentTreeBOList.stream().filter(bo -> bo.getParentCode().equals(departmentTreeBO.getCode())).collect(Collectors.toList());
		if (!CollectionUtils.isEmpty(children)){
			children.forEach(bo -> {
				bo.setChildren(getDepartmentChildren(departmentTreeBOList, bo));
			});
		}
		return children;
	}

	/**
	 * 根据部门编码查询部门名称
	 * @param deptCode
	 * @return
	 */
	public String getDepartmentNameByDeptCode(String deptCode) {
		DepartmentPO departmentPO = departmentPOMapper.selectOne(new LambdaQueryWrapper<DepartmentPO>().eq(DepartmentPO::getDeptCode,deptCode));
		return departmentPO.getDepartmentName();
	}

	/**
	 * 查询以deptCode为parentCode的所有部门列表
	 * @param deptCode
	 * @return
	 */
	public List<DepartmentPO> getChildDepartmentByParentDeptCode(String deptCode) {
		return departmentPOMapper.selectList(new LambdaQueryWrapper<DepartmentPO>()
				.eq(DepartmentPO::getParentCode,deptCode));
	}
}

