package com.cf.parking.services.facade.impl;

import java.util.List;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.ParkingSpaceTransferRecordMapper;
import com.cf.parking.dao.po.ParkingSpaceTransferRecordPO;
import com.cf.parking.facade.bo.ParkingSpaceTransferRecordBO;
import com.cf.parking.facade.dto.ParkingSpaceTransferRecordDTO;
import com.cf.parking.facade.facade.ParkingSpaceTransferRecordFacade;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 车位转赠记录Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Service
public class ParkingSpaceTransferRecordFacadeImpl implements ParkingSpaceTransferRecordFacade
{
    @Autowired
    private ParkingSpaceTransferRecordMapper mapper;

    /**
     * 查询车位转赠记录列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<ParkingSpaceTransferRecordBO> getParkingSpaceTransferRecordList(ParkingSpaceTransferRecordDTO dto) {
        Page<ParkingSpaceTransferRecordPO> page = PageUtils.toPage(dto);

        Page<ParkingSpaceTransferRecordPO> poPage = mapper.selectPage(page, new LambdaQueryWrapper<ParkingSpaceTransferRecordPO>()
                .eq(ObjectUtils.isNotEmpty(dto.getUserId()), ParkingSpaceTransferRecordPO::getUserId, dto.getUserId())
                .le(ObjectUtils.isNotEmpty(dto.getValidEndDate()), ParkingSpaceTransferRecordPO::getValidEndDate, dto.getValidEndDate())
                .ge(ObjectUtils.isNotEmpty(dto.getValidStartDate()), ParkingSpaceTransferRecordPO::getValidStartDate, dto.getValidStartDate())
                .orderByDesc(ParkingSpaceTransferRecordPO::getCreateTm));

        List<ParkingSpaceTransferRecordBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), ParkingSpaceTransferRecordBO.class);
        return PageUtils.toResponseList(page,boList);
    }

}
