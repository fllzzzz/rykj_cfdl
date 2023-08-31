package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.ParkingEvaluatePO;
import org.apache.ibatis.annotations.Param;

public interface ParkingEvaluatePOMapper extends BaseMapper<ParkingEvaluatePO> {
    int deleteByPrimaryKey(Long parkingEvaluateId);

    int insertSelective(ParkingEvaluatePO record);

    ParkingEvaluatePO selectByPrimaryKey(Long parkingEvaluateId);

    int updateByPrimaryKeySelective(@Param("record") ParkingEvaluatePO record);

    int updateByPrimaryKey(ParkingEvaluatePO record);

    Float selectAvg(@Param("record") ParkingEvaluatePO record);

}