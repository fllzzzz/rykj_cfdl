package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.BlackListPO;
import org.apache.ibatis.annotations.Mapper;

/**
 * @author lpy
 * @date 2023-03-27 09:43:43
 * @description 黑名单记录表
 */
@Mapper
public interface BlackListMapper extends BaseMapper<BlackListPO> {

}

