package com.cf.parking.services.integration;

import javax.annotation.Resource;
import org.apache.http.HttpEntity;
import org.apache.http.util.EntityUtils;
import org.springframework.stereotype.Service;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import com.cf.parking.facade.bo.ParkBaseDetailRespBO;
import com.cf.parking.facade.bo.ParkBaseRespBO;
import com.cf.parking.facade.bo.YardDetailBO;
import com.cf.parking.facade.bo.YardPageBO;
import com.cf.parking.facade.dto.Carmanagement;
import com.cf.parking.facade.dto.QueryYardDTO;
import com.cf.parking.services.enums.ParkingRemoteCodeEnum;
import com.cf.parking.services.properties.ParkingProperties;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.utils.HttpClientUtil;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ParkInvokeService {

	@Resource
	private ParkingProperties parkingProperties;
	
	
	/**
	 * 新增或修改车辆信息
	 * @param info
	 * @return
	 */
	public boolean replaceCarInfo(Carmanagement info) {
		try {
			AssertUtil.checkNull(info, "参数不能为空");
			AssertUtil.checkNull(info.getLicensePlate(), "车牌号不能为空");
			AssertUtil.checkNull(info.getParkIndexCode(), "车库不能为空");
			AssertUtil.checkNull(info.getCarOwner(), "车主不能为空");
			AssertUtil.checkNull(info.getJobNo(), "工号不能为空");
			AssertUtil.checkNull(info.getPermissStart(), "权限期限开始时间不能为空");
			AssertUtil.checkNull(info.getLicensePlate(), "权限期限结束时间不能为空");
			HttpEntity entity = HttpClientUtil.doPostStr(parkingProperties.getHost() + parkingProperties.getAddCarmanagementUrl(), JSON.toJSONString(info));
			String reponseContent = EntityUtils.toString(entity,"UTF-8");
			log.info("调用添加车辆接口入参：{},出参:{}",JSON.toJSONString(info),JSON.toJSONString(reponseContent));
			ParkBaseRespBO<ParkBaseDetailRespBO> resp = JSON.parseObject(reponseContent, new TypeReference<ParkBaseRespBO<ParkBaseDetailRespBO>>() {} );
			if(ParkingRemoteCodeEnum.RESP_SUCCESS.getState().equals(resp.getResCode()) && resp.getResult() != null && 
					ParkingRemoteCodeEnum.BUS_CODE.getState().equals(resp.getResult().getCode())) {
				return true;
			}
			return false;
		} catch (Exception e) {
			log.info("调用添加车辆接口入参：{},e={}",JSON.toJSONString(info),e);
			return false;
		}
	}
	
	
	public YardPageBO queryYard(QueryYardDTO dto) {
		
		try {
			AssertUtil.checkNull(dto, "参数不能为空");
			HttpEntity entity = HttpClientUtil.doPostStr(parkingProperties.getHost() + parkingProperties.getQueryyardUrl(), JSON.toJSONString(dto));
			String reponseContent = EntityUtils.toString(entity,"UTF-8");
			log.info("调用查询车库接口入参：{},出参:{}",JSON.toJSONString(dto),JSON.toJSONString(reponseContent));
			ParkBaseRespBO<YardPageBO> resp = JSON.parseObject(reponseContent, new TypeReference<ParkBaseRespBO<YardPageBO>>() {} );
			if(ParkingRemoteCodeEnum.RESP_SUCCESS.getState().equals(resp.getResCode()) && resp.getResult() != null && 
					ParkingRemoteCodeEnum.BUS_CODE.getState().equals(resp.getResult().getCode())) {
				YardPageBO result = resp.getResult();
				result.setData(JSON.parseArray(JSON.toJSONString(result.getData()), YardDetailBO.class));
				return result;
			}
			return null;
		} catch (Exception e) {
			log.info("调用查询车库接口入参：{},e:{}",JSON.toJSONString(dto),e);
			return null;
		}
	}
	
}
