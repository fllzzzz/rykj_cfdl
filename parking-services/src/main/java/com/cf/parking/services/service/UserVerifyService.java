package com.cf.parking.services.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.po.UserVerifyPO;
import com.cf.parking.dao.mapper.UserVerifyMapper;
import org.springframework.stereotype.Service;

/**
 * @author whx
 * @date 2022-11-19 16:55:55
 * @description 车主认证表
 */
@Service
public class UserVerifyService extends ServiceImpl<UserVerifyMapper, UserVerifyPO> implements IService<UserVerifyPO> {

}

