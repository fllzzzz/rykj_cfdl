package com.cf.parking.services.service;


import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.UserPOMapper;
import com.cf.parking.dao.po.UserPO;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Collection;
import java.util.List;


@Service
public class UserService extends ServiceImpl<UserPOMapper, UserPO>
        implements IService<UserPO> {

    @Resource
    private UserPOMapper userPOMapper;

    /**
     * 根据openId查询用户信息
     */
    public UserPO selectByOpenId(String openId){
        return userPOMapper.selectByOpenId(openId);
    }

    /**
     * 用户信息保存
     */
    public void saveUser(UserPO userPO){
        userPOMapper.insertSelective(userPO);
    }

    /**
     * 用户信息更新
     */
    public void updateUserInfo(UserPO userPO){
        userPOMapper.updateByPrimaryKeySelective(userPO);
    }

    /**
     * 根据用户id列表 查询用户信息
     *
     * @param userIdList 用户id列表
     * @return 用户信息
     */
    public List<UserPO> getUserByUserIdList(Collection<Long> userIdList) {
        return list(new LambdaQueryWrapper<UserPO>().in( UserPO::getUserId, userIdList));
    }
    
    
    public List<UserPO> getUserByOpenIdList(Collection<String> openIdList) {
        return list(new LambdaQueryWrapper<UserPO>().in( UserPO::getOpenId, openIdList));
    }
}
