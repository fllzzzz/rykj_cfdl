package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.po.UserVerifyPO;
import com.cf.parking.services.enums.UserVerifyStateEnum;
import com.cf.parking.dao.mapper.UserVerifyMapper;

import java.util.List;

import javax.annotation.Resource;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

/**
 * @author whx
 * @date 2022-11-19 16:55:55
 * @description 车主认证表
 */
@Service
public class UserVerifyService extends ServiceImpl<UserVerifyMapper, UserVerifyPO> implements IService<UserVerifyPO> {

	
	@Resource
	private UserVerifyMapper userVerifyMapper;
	
	
	
	
	/**
	 * 根据用户ID查询认证车牌信息
	 * @param userIdList
	 * @return
	 */
	public List<UserVerifyPO> queryVerifyListByUserIdList(List<Long> userIdList) {
		return CollectionUtils.isEmpty(userIdList) ? null : userVerifyMapper.selectList(new LambdaQueryWrapper<UserVerifyPO>()
					.in(UserVerifyPO::getUserId, userIdList)
					.eq(UserVerifyPO::getState, UserVerifyStateEnum.SUCCESS.getState())
				);
	}

}

