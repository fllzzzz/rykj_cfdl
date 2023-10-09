package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.ParkingSpaceChangeRecordPO;



/**
 * @author think
 * 车位交换
 */
public interface ParkingSpaceChangeRecordPOMapper extends BaseMapper<ParkingSpaceChangeRecordPO>{
    int deleteByPrimaryKey(Long id);

    int insert(ParkingSpaceChangeRecordPO record);

    int insertSelective(ParkingSpaceChangeRecordPO record);

    ParkingSpaceChangeRecordPO selectByPrimaryKey(Long id);

    int updateByPrimaryKeySelective(ParkingSpaceChangeRecordPO record);

    int updateByPrimaryKey(ParkingSpaceChangeRecordPO record);
}