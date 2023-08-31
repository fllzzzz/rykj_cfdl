package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.BlacklistSettingsPO;
import org.apache.ibatis.annotations.Mapper;

/**
 * @author lpy
 * @date 2023-03-27 10:29:20
 * @description 黑名单设置表
 */
@Mapper
public interface BlacklistSettingsMapper extends BaseMapper<BlacklistSettingsPO> {

}

