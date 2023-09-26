package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.ParkingLotPO;
import org.apache.ibatis.annotations.Param;

/**
 * 停车场主Mapper接口
 *
 * @author
 * @date 2023-09-05
 */
public interface ParkingLotMapper extends BaseMapper<ParkingLotPO> {

    /**
     * 根据id查询包含图片信息的停车场对象信息
     *
     * @param id
     * @return
     */
    ParkingLotPO selectParkingLotPOWithImageInfoById(@Param("id") Long id);

    /**
     * 新增带图片信息的停车场信息
     * @param po
     * @return
     */
    int insertWithImageInfo(ParkingLotPO po);

    /**
     * 修改带图片信息的停车场信息
     * @param po
     * @return
     */
    int updateWithImageInfo(ParkingLotPO po);
}
