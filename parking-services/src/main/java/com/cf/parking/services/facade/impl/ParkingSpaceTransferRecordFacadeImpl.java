package com.cf.parking.services.facade.impl;

import java.util.List;

import com.cf.parking.dao.mapper.ParkingSpaceTransferRecordMapper;
import com.cf.parking.facade.facade.ParkingSpaceTransferRecordFacade;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 车位转赠记录Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Service
public class ParkingSpaceTransferRecordFacadeImpl implements ParkingSpaceTransferRecordFacade
{
    @Autowired
    private ParkingSpaceTransferRecordMapper parkingSpaceTransferRecordMapper;

    /**
     * 查询车位转赠记录
     * 
     * @param id 车位转赠记录主键
     * @return 车位转赠记录
     */
//    @Override
//    public ParkingSpaceTransferRecord selectParkingSpaceTransferRecordById(Long id)
//    {
//        return parkingSpaceTransferRecordMapper.selectParkingSpaceTransferRecordById(id);
//    }

    /**
     * 查询车位转赠记录列表
     * 
     * @param parkingSpaceTransferRecord 车位转赠记录
     * @return 车位转赠记录
     */
//    @Override
//    public List<ParkingSpaceTransferRecord> selectParkingSpaceTransferRecordList(ParkingSpaceTransferRecord parkingSpaceTransferRecord)
//    {
//        return parkingSpaceTransferRecordMapper.selectParkingSpaceTransferRecordList(parkingSpaceTransferRecord);
//    }

    /**
     * 新增车位转赠记录
     * 
     * @param parkingSpaceTransferRecord 车位转赠记录
     * @return 结果
     */
//    @Override
//    public int insertParkingSpaceTransferRecord(ParkingSpaceTransferRecord parkingSpaceTransferRecord)
//    {
//        return parkingSpaceTransferRecordMapper.insertParkingSpaceTransferRecord(parkingSpaceTransferRecord);
//    }

    /**
     * 修改车位转赠记录
     * 
     * @param parkingSpaceTransferRecord 车位转赠记录
     * @return 结果
     */
//    @Override
//    public int updateParkingSpaceTransferRecord(ParkingSpaceTransferRecord parkingSpaceTransferRecord)
//    {
//        return parkingSpaceTransferRecordMapper.updateParkingSpaceTransferRecord(parkingSpaceTransferRecord);
//    }

    /**
     * 批量删除车位转赠记录
     * 
     * @param ids 需要删除的车位转赠记录主键
     * @return 结果
     */
//    @Override
//    public int deleteParkingSpaceTransferRecordByIds(Long[] ids)
//    {
//        return parkingSpaceTransferRecordMapper.deleteParkingSpaceTransferRecordByIds(ids);
//    }

    /**
     * 删除车位转赠记录信息
     * 
     * @param id 车位转赠记录主键
     * @return 结果
     */
//    @Override
//    public int deleteParkingSpaceTransferRecordById(Long id)
//    {
//        return parkingSpaceTransferRecordMapper.deleteParkingSpaceTransferRecordById(id);
//    }
}
