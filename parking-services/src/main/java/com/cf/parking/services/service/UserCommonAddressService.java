package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.po.UserCommonAddressPO;
import com.cf.parking.dao.mapper.UserCommonAddressMapper;
import com.cf.parking.facade.bo.UserCommonAddressBO;
import com.cf.parking.facade.dto.CommonAddressDTO;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author whx
 * @date 2022-11-11 14:22:44
 * @description 用户常用地址表
 */
@Service
public class UserCommonAddressService extends ServiceImpl<UserCommonAddressMapper, UserCommonAddressPO> implements IService<UserCommonAddressPO> {
	@Resource
	private UserCommonAddressMapper userCommonAddressMapper;

	/**
	 * 新增常用地址
	 *
	 * @param param
	 * @return
	 */
	public Result addAddress(CommonAddressDTO param, Long userId) {
		//常用地址:每个用户限制5
		if (this.count(new LambdaQueryWrapper<UserCommonAddressPO>().eq(UserCommonAddressPO::getUserId, userId)) >= 5L) {
			throw new BusinessException(BizResultCodeEnum.COMMON_ADDRESS_LIMIT.getMsg());
		}
		UserCommonAddressPO commonAddressPO = BeanConvertorUtils.map(param, UserCommonAddressPO.class).setUserId(userId);
		this.save(commonAddressPO);
		return Result.buildSuccessResult();
	}

	/**
	 * 删除常用地址
	 *
	 * @return
	 */
	public Result delAddress(Long userCommonAddressId, Long userId) {
		//权限校验
		if (!userId.equals(this.getById(userCommonAddressId).getUserId())) {
			throw new BusinessException(BizResultCodeEnum.USER_ERROR.getMsg());
		}
		this.removeById(userCommonAddressId);
		return Result.buildSuccessResult();
	}

	/**
	 * 获取常用地址
	 *
	 * @return
	 */
	public List<UserCommonAddressBO> getAddress(Long userId) {
		List<UserCommonAddressPO> userCommonAddressPOS = userCommonAddressMapper.selectList(new LambdaQueryWrapper<UserCommonAddressPO>()
				.eq(UserCommonAddressPO::getUserId, userId));
		return BeanConvertorUtils.copyList(userCommonAddressPOS, UserCommonAddressBO.class);
	}
}

