package com.cf.parking.services.service;

import java.util.List;

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
	
	public List<ParkingLotPO> selectParkingLotByCodes(List<String> parkingList) {
		log.info("根据编码查询停车场:param={}",parkingList);
		return parkingLotMapper.selectList(new LambdaQueryWrapper<ParkingLotPO>()
					.in(ParkingLotPO::getRegionCode, parkingList)
				);
	}

	
}
