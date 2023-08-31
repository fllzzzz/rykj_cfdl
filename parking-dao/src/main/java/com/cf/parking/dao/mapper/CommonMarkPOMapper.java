package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.CommonMarkPO;

public interface CommonMarkPOMapper extends BaseMapper<CommonMarkPO> {
    int deleteByPrimaryKey(Long commonMarkId);

    int insertSelective(CommonMarkPO record);

    CommonMarkPO selectByPrimaryKey(Long commonMarkId);

    int updateByPrimaryKeySelective(CommonMarkPO record);

    int updateByPrimaryKey(CommonMarkPO record);
}