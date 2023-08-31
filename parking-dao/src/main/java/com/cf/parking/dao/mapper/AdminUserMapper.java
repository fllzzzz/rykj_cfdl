package com.cf.parking.dao.mapper;


import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.AdminUser;
import org.apache.ibatis.annotations.Param;

public interface AdminUserMapper extends BaseMapper<AdminUser> {

    AdminUser selectByEmplNoAndState(String emplNo);

    int updatePwd(@Param("password") String password, @Param("adminUserId") Long adminUserId);
}