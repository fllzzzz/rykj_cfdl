package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.ScheduleDataPO;
import org.apache.ibatis.annotations.Mapper;

/**
 * @author whx
 * @date 2023-03-27 09:31:03
 * @description 排班记录表
 */
@Mapper
public interface ScheduleDataMapper extends BaseMapper<ScheduleDataPO> {

}