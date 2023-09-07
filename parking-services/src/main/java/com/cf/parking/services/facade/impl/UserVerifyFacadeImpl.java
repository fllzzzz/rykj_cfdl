package com.cf.parking.services.facade.impl;

import com.cf.parking.dao.mapper.UserVerifyMapper;
import com.cf.parking.facade.facade.UserVerifyFacade;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 车辆审核 Service业务层处理
 * @author
 * @date 2023/9/7
 */
@Service
public class UserVerifyFacadeImpl implements UserVerifyFacade {

    @Autowired
    private UserVerifyMapper mapper;
}
