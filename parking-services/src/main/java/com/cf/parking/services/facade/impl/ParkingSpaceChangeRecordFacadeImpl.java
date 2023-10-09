package com.cf.parking.services.facade.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.ParkingSpaceChangeRecordPOMapper;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.dao.po.ParkingSpaceChangeRecordPO;
import com.cf.parking.dao.po.ParkingSpaceTransferRecordPO;
import com.cf.parking.facade.bo.ParkingSpaceChangeRecordBO;
import com.cf.parking.facade.bo.ParkingSpaceTransferRecordBO;
import com.cf.parking.facade.dto.ParkingSpaceChangeRecordDTO;
import com.cf.parking.facade.facade.ParkingSpaceChangeRecordFacade;
import com.cf.parking.services.service.ParkingLotService;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;

import lombok.extern.slf4j.Slf4j;



@Slf4j
@Service
public class ParkingSpaceChangeRecordFacadeImpl implements ParkingSpaceChangeRecordFacade {

	@Resource
	private ParkingSpaceChangeRecordPOMapper changeRecordPOMapper;
	
	
	@Resource
	private ParkingLotService parkingLotService;
	
	
	
	
	@Override
	public PageResponse<ParkingSpaceChangeRecordBO> getParkingSpaceChangeRecordList(ParkingSpaceChangeRecordDTO dto) {
		log.info("查询互换记录params:{}",JSON.toJSONString(dto));
		Page<ParkingSpaceChangeRecordPO> page = PageUtils.toPage(dto);
		
        Page<ParkingSpaceChangeRecordPO> poPage = changeRecordPOMapper.selectPage(page, 
        		new LambdaQueryWrapper<ParkingSpaceChangeRecordPO>()
                .like(ObjectUtils.isNotEmpty(dto.getAcceptUserName()), ParkingSpaceChangeRecordPO::getAcceptUserName, dto.getAcceptUserName())
                .like(ObjectUtils.isNotEmpty(dto.getUserName()), ParkingSpaceChangeRecordPO::getUserName, dto.getUserName())
                .and(ObjectUtils.isNotEmpty(dto.getUserId()), 
                		i -> i.eq(ObjectUtils.isNotEmpty(dto.getUserId()), ParkingSpaceChangeRecordPO::getAcceptUserId, dto.getUserId())
                			.or()
                			.eq(ObjectUtils.isNotEmpty(dto.getUserId()), ParkingSpaceChangeRecordPO::getUserId, dto.getUserId()))
                .orderByDesc(ParkingSpaceChangeRecordPO::getCreateTm));

        List<ParkingSpaceChangeRecordBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), ParkingSpaceChangeRecordBO.class);
		Map<String,String> parkingMap = new HashMap<>();
        boList.forEach(item -> setParkingLotRegionByCode(item,parkingMap));
		return PageUtils.toResponseList(page,boList);
	}

	
	private void setParkingLotRegionByCode(ParkingSpaceChangeRecordBO bo, Map<String, String> parkingMap) {
		ParkingLotPO parkingLotPO = null;
		String parkingName = parkingMap.get(bo.getParkingCode());
		if (StringUtils.isEmpty(parkingName)){
			parkingLotPO = parkingLotService.selectParkingLotByCode(bo.getParkingCode());
			if (parkingLotPO != null) {
				parkingName = parkingLotPO.getRegion();
				parkingMap.put(bo.getParkingCode(), parkingName);
			}
		}
		bo.setParkingName(parkingName);
		
		String acceptParkingName = parkingMap.get(bo.getAcceptParkingCode());
		if (StringUtils.isEmpty(acceptParkingName)){
			parkingLotPO = parkingLotService.selectParkingLotByCode(bo.getAcceptParkingCode());
			if (parkingLotPO != null) {
				acceptParkingName = parkingLotPO.getRegion();
				parkingMap.put(bo.getAcceptParkingCode(), acceptParkingName);
			}
		}
		bo.setAcceptParkingName(acceptParkingName);
	}
}
