package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.AdminUserMapper;
import com.cf.parking.dao.po.AdminUser;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author weihui
 * @date 2020/10/17
 */
@Service
public class AdminUserService extends ServiceImpl<AdminUserMapper, AdminUser> implements IService<AdminUser> {

    @Resource
    private AdminUserMapper adminUserMapper;

    public AdminUser getUserByEmplNo(String emplNo) {
        AdminUser adminUser = adminUserMapper.selectByEmplNoAndState(emplNo);
        return adminUser;
    }

    public int updatePwd(String password, Long adminUserId) {
        return adminUserMapper.updatePwd(password, adminUserId);
    }

    public List<AdminUser> getAdminUserList(List<Long> adminUserIdList) {
        return adminUserMapper.selectList(new LambdaQueryWrapper<AdminUser>().in(AdminUser::getAdminUserId, adminUserIdList));
    }
}
