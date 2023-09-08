package com.cf.parking.services.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.po.DepartmentPO;
import com.cf.parking.dao.mapper.DepartmentMapper;
import org.springframework.stereotype.Service;

/**
 * @author 
 * @date 2023-09-08 13:53:13
 *
 * @description 部门表
 */
@Service
public class DepartmentService extends ServiceImpl<DepartmentMapper, DepartmentPO>  implements IService<DepartmentPO> {

}

