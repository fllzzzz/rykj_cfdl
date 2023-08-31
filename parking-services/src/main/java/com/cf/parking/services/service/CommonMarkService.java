package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.CommonMarkPOMapper;
import com.cf.parking.dao.po.CommonMarkPO;
import org.springframework.stereotype.Service;

/**
 * @author: lpy
 * @Date: 2022/11/18
 */
@Service
public class CommonMarkService extends ServiceImpl<CommonMarkPOMapper, CommonMarkPO> implements IService<CommonMarkPO> {
    public CommonMarkPO getRecordBySourceTypeSourceCodeAndState(CommonMarkPO commonMarkPO) {
        return this.getOne(new LambdaQueryWrapper<CommonMarkPO>()
                .eq(CommonMarkPO::getSourceCode, commonMarkPO.getSourceCode())
                .eq(CommonMarkPO::getSourceType, commonMarkPO.getSourceType())
                .eq(CommonMarkPO::getState, commonMarkPO.getState()));
    }
}
