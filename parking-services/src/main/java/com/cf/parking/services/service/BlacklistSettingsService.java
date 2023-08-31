package com.cf.parking.services.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.po.BlacklistSettingsPO;
import com.cf.parking.dao.mapper.BlacklistSettingsMapper;
import org.springframework.stereotype.Service;

/**
 * @author lpy
 * @date 2023-03-27 10:29:20
 * @description 黑名单设置表
 */
@Service
public class BlacklistSettingsService extends ServiceImpl<BlacklistSettingsMapper, BlacklistSettingsPO> implements IService<BlacklistSettingsPO> {

}

