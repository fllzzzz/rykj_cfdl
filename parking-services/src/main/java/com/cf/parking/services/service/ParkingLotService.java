package com.cf.parking.services.service;

import javax.annotation.Resource;
import org.springframework.stereotype.Service;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.ParkingLotMapper;
import com.cf.parking.dao.po.ParkingLotPO;
import lombok.extern.slf4j.Slf4j;



@Service
@Slf4j
public class ParkingLotService extends ServiceImpl<ParkingLotMapper, ParkingLotPO> implements IService<ParkingLotPO>{

	@Resource
	private ParkingLotMapper parkingLotMapper;
	
	/**
	 * 根据停车场编码查询停车场
	 * @param parkCode
	 * @return
	 */
	public ParkingLotPO selectParkingLotByCode(String parkCode) {
		log.info("根据编码查询停车场:param={}",parkCode);
		return parkingLotMapper.selectOne(new LambdaQueryWrapper<ParkingLotPO>()
					.eq(ParkingLotPO::getRegionCode, parkCode)
				);
	}

	
}
