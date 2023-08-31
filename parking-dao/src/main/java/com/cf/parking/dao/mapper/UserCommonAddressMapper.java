package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.UserCommonAddressPO;
import org.apache.ibatis.annotations.Mapper;

/**
 * @author whx
 * @date 2022-11-11 14:22:44
 * @description 用户常用地址表
 */
@Mapper
public interface UserCommonAddressMapper extends BaseMapper<UserCommonAddressPO> {

}

