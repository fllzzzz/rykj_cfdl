package com.cf.parking.services.service;


import com.cf.parking.dao.mapper.AdminOptLogMapper;
import com.cf.parking.dao.po.AdminOptLog;
import com.cf.support.authertication.AdminAuthenticationServer;
import com.cf.support.authertication.token.dto.AdminSessionDTO;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * @author weihui
 * @date 2020/10/20
 */
@Service
public class AdminOptLogService {
    @Resource
    private AdminOptLogMapper adminOptLogMapper;

    @Resource
    private AdminAuthenticationServer adminAuthenticationServer;

    @Async
    public void save(AdminOptLog item, String token) {
        AdminSessionDTO currentUser = adminAuthenticationServer.getCurrentUser(token);
        item.setAdminUserId(Long.parseLong(currentUser.getAdminId()));
        item.setAdminName(currentUser.getAdminName());

        adminOptLogMapper.insertSelective(item);
    }
}
