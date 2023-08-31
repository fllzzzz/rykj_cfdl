package com.cf.parking.dao.mapper;


import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.UserPO;

public interface UserPOMapper extends BaseMapper<UserPO> {

    int insertSelective(UserPO record);

    int updateByPrimaryKeySelective(UserPO record);

    UserPO selectByOpenId(String openId);
}