package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.NotParkingRecordPO;
import org.apache.ibatis.annotations.Mapper;

/**
 * @author csy
 * @date 2023-03-27 09:16:26
 *
 * @description 僵尸车扫描记录表
 */
@Mapper
public interface NotParkingRecordMapper extends BaseMapper<NotParkingRecordPO> {

}

