package com.cf.parking.services.facade.impl;

import java.util.List;

import com.cf.parking.dao.mapper.ParkingLotMapper;
import com.cf.parking.facade.facade.ParkingLotFacade;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 停车场主Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Service
public class ParkingLotServiceImpl implements ParkingLotFacade
{
    @Autowired
    private ParkingLotMapper parkingLotMapper;

    /**
     * 查询停车场主
     * 
     * @param id 停车场主主键
     * @return 停车场主
     */
//    @Override
//    public ParkingLot selectParkingLotById(Long id)
//    {
//        return parkingLotMapper.selectParkingLotById(id);
//    }

    /**
     * 查询停车场主列表
     * 
     * @param parkingLot 停车场主
     * @return 停车场主
     */
//    @Override
//    public List<ParkingLot> selectParkingLotList(ParkingLot parkingLot)
//    {
//        return parkingLotMapper.selectParkingLotList(parkingLot);
//    }

    /**
     * 新增停车场主
     * 
     * @param parkingLot 停车场主
     * @return 结果
     */
//    @Override
//    public int insertParkingLot(ParkingLot parkingLot)
//    {
//        return parkingLotMapper.insertParkingLot(parkingLot);
//    }

    /**
     * 修改停车场主
     * 
     * @param parkingLot 停车场主
     * @return 结果
     */
//    @Override
//    public int updateParkingLot(ParkingLot parkingLot)
//    {
//        return parkingLotMapper.updateParkingLot(parkingLot);
//    }

    /**
     * 批量删除停车场主
     * 
     * @param ids 需要删除的停车场主主键
     * @return 结果
     */
//    @Override
//    public int deleteParkingLotByIds(Long[] ids)
//    {
//        return parkingLotMapper.deleteParkingLotByIds(ids);
//    }

    /**
     * 删除停车场主信息
     * 
     * @param id 停车场主主键
     * @return 结果
     */
//    @Override
//    public int deleteParkingLotById(Long id)
//    {
//        return parkingLotMapper.deleteParkingLotById(id);
//    }
}
