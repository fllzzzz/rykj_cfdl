package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.ParkingLotAreaBO;
import com.cf.parking.facade.bo.ParkingLotBO;
import com.cf.parking.facade.bo.ParkingLotTreeBO;
import com.cf.parking.facade.dto.ParkingLotAreaOptDTO;
import com.cf.parking.facade.dto.ParkingLotDTO;
import com.cf.parking.facade.dto.ParkingLotOptDTO;
import com.cf.support.result.PageResponse;

import java.util.List;


/**
 * 停车场主Service接口
 * 
 * @author
 * @date 2023-09-05
 */
public interface ParkingLotFacade
{

    /**
     * 查询停车场列表
     * @param dto
     * @return
     */
    PageResponse<ParkingLotBO> getParkingLotList(ParkingLotDTO dto);

    /**
     * 新增停车场
     * @param dto
     * @return
     */
    Integer add(ParkingLotOptDTO dto);

    /**
     * 修改停车场
     * @param dto
     * @return
     */
    Integer update(ParkingLotOptDTO dto);

    /**
     * 删除停车场
     * @param id
     * @return
     */
    Integer deleteById(Long id);

    /**
     * 查询园区列表
     * @return
     */
    List<ParkingLotAreaBO> getAreaList(String region);

    /**
     * 新增园区
     * @param dto
     * @return
     */
    Integer addArea(ParkingLotAreaOptDTO dto);

    /**
     * 修改园区
     * @param dto
     * @return
     */
    Integer updateArea(ParkingLotAreaOptDTO dto);

    /**
     * 查询停车场树形列表
     * @return
     */
    List<ParkingLotTreeBO> getParkingLotTreeList();

}
