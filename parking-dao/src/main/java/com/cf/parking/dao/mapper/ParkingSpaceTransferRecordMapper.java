package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.dto.ParkingSpaceTransferRecordRspDTO;
import com.cf.parking.dao.po.ParkingSpaceTransferRecordPO;
import org.apache.ibatis.annotations.Param;

import java.util.Date;
import java.util.List;

/**
 * 车位转赠记录Mapper接口
 *
 * @author
 * @date 2023-09-05
 */
public interface ParkingSpaceTransferRecordMapper extends BaseMapper<ParkingSpaceTransferRecordPO> {


    /**
     * PC端根据传入参数分页查询车位转增记录列表
     * @param page
     * @param queryPO
     * @param userInfo
     * @return
     */
    IPage<ParkingSpaceTransferRecordRspDTO> selectParkingSpaceTransferRecordPage(Page<?> page, @Param("queryPO") ParkingSpaceTransferRecordPO queryPO, @Param("userInfo") String userInfo);
}
