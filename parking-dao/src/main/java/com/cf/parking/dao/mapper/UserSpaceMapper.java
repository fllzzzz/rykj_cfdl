package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.UserSpacePO;
import org.apache.ibatis.annotations.Mapper;

/**
 * @author lpy
 * @date 2023-03-27 16:56:05
 * @description 用户车位表
 */
@Mapper
public interface UserSpaceMapper extends BaseMapper<UserSpacePO> {
    /**
     * 清除库中数据，清空id
     */
    void deleteAll();
}

