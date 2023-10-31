package com.cf.parking.services.service;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.springframework.stereotype.Service;
import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.ParkingInitMapper;
import com.cf.parking.dao.po.ParkingInitPO;
import lombok.extern.slf4j.Slf4j;



@Service
@Slf4j
public class ParkingInitService extends ServiceImpl<ParkingInitMapper, ParkingInitPO> implements IService<ParkingInitPO>{

	@Resource
	private ParkingInitMapper parkingInitMapper;
	

	/**
	 * 查询所有的车库闸机信息
	 * @return
	 */
	public Map<String,String> queryAllParking(){
	 	List<ParkingInitPO> parkingList = parkingInitMapper.selectList(null);
	 	log.info("闸机数据：{}",JSON.toJSONString(parkingList));
	 	Map<String,String> map = parkingList.stream().collect(Collectors.toMap(ParkingInitPO::getRegionCode, ParkingInitPO::getRegion));
	 	return map;
	}
	
	
	/**
	 * 根据编码查询车库名称
	 * @return
	 */
	public String queryParkingNameByCode(String parkingCode){
	 	ParkingInitPO parkingList = parkingInitMapper.selectOne(
	 			new LambdaQueryWrapper<ParkingInitPO>()
	 			.eq(ParkingInitPO::getRegionCode, parkingCode)
	 		);
	 	log.info("闸机数据：{}",JSON.toJSONString(parkingList));
	 	return parkingList == null ? "" : parkingList.getRegion();
	}
	
}
