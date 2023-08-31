package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.po.ParkingSpacePO;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface ParkingSpacePOMapper {
    int deleteByPrimaryKey(Integer id);

    int insertSelective(ParkingSpacePO record);

    ParkingSpacePO selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(ParkingSpacePO record);

    int updateByPrimaryKey(ParkingSpacePO record);

    int updateByPersonIdSelective(@Param("list") List<ParkingSpacePO> list);

    List<ParkingSpacePO> getTempCarInRecords(ParkingSpacePO record);

    int updateByPlateNoSelective(@Param("stateList") List<ParkingSpacePO> stateList);

    List<ParkingSpacePO> getAllTempCarInRecords();
    IPage<ParkingSpacePO> getTempCarInRecords(Page<?> page, @Param("item") ParkingSpacePO record);

    int clearAll();
}