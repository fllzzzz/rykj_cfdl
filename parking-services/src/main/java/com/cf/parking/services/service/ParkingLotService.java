package com.cf.parking.services.service;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
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
	
	private final String REGIONCODE = "regionCode";
	
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


	public List<String> queryParentListViaSelf(String parkCode) {
		List<String> list = new ArrayList<>();
		ParkingLotPO parking = selectParkingLotByCode(parkCode);
		if (parking == null ) {
			return list;
		}
		recursionParking(list, parking.getId());
		return list;
	}
	
	
	/**
	 * 递归获取上级停车场编码和园区编码
	 * @param result
	 * @param parkId
	 */
	private void recursionParking(List<String> result ,Long parkId){
		ParkingLotPO parking = parkingLotMapper.selectById(parkId);
		if (parking == null) {
			return;
		}
		
		if (parking.getParentId() == 0) {
			List<JSONObject> jsonList = JSON.parseArray(parking.getRegionCode(), JSONObject.class);
			if(!CollectionUtils.isEmpty(jsonList)) {
				jsonList.forEach(item -> result.add(item.getString(REGIONCODE)));
			}
		} else {
			result.add(parking.getRegionCode());
			recursionParking(result,parking.getParentId());
		}
	}
}
