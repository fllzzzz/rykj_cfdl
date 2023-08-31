package com.cf.parking.dao.mapper;


import com.cf.parking.dao.po.ParkingFreePO;
import com.cf.parking.dao.query.ParkingFreeQuery;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface ParkingFreePOMapper {

    int insertList(@Param("list") List<ParkingFreePO> list);

    List<ParkingFreePO> getList(@Param("item") ParkingFreeQuery item);
}
