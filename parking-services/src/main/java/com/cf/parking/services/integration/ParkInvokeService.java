package com.cf.parking.services.integration;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.annotation.Resource;
import org.springframework.stereotype.Service;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import com.cf.parking.facade.bo.ParkBaseDetailRespBO;
import com.cf.parking.facade.bo.ParkBaseRespBO;
import com.cf.parking.facade.bo.ParkingCarInfoBO;
import com.cf.parking.facade.bo.ParkingCarQueryRespBO;
import com.cf.parking.facade.bo.ParkingTokenBO;
import com.cf.parking.facade.bo.ParkingYardBO;
import com.cf.parking.facade.bo.YardDetailBO;
import com.cf.parking.facade.bo.YardPageBO;
import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.facade.dto.Carmanagement;
import com.cf.parking.facade.dto.ParkingCarQueryDTO;
import com.cf.parking.facade.dto.QueryYardDTO;
import com.cf.parking.facade.dto.UserSpaceDTO;
import com.cf.parking.services.enums.ParkingRemoteCodeEnum;
import com.cf.parking.services.properties.ParkingProperties;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.exception.BusinessException;
import com.cf.support.utils.HttpClientUtil;
import com.cf.support.utils.RedisUtil;

import cn.hutool.core.date.DateUtil;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ParkInvokeService {

	@Resource
	private ParkingProperties parkingProperties;
	
	@Resource
	private RedisUtil redisUtil;

	/**
	 * 新增或修改车辆信息
	 * @param info
	 * @return
	 */
	public ParkBaseRespBO<ParkBaseDetailRespBO> replaceCarInfo(UserSpaceDTO space) {
		Carmanagement info = new Carmanagement();
		info.setLicensePlate(space.getPlateNo())
			.setJobNo(space.getJobNumber())
			.setParkIndexCode(space.getParkingLot().split(","))
			.setCarOwner(space.getName())
			.setPermissStart(DateUtil.format(DateUtil.beginOfDay(space.getStartDate()), "yyyy-MM-dd HH:mm:ss") )
			.setPermissEnd(DateUtil.format(DateUtil.endOfDay(space.getEndDate()), "yyyy-MM-dd HH:mm:ss"));
		try {
			AssertUtil.checkNull(info, "参数不能为空");
			AssertUtil.checkNull(info.getLicensePlate(), "车牌号不能为空");
			AssertUtil.checkNull(info.getParkIndexCode(), "车库不能为空");
			AssertUtil.checkNull(info.getCarOwner(), "车主不能为空");
			AssertUtil.checkNull(info.getJobNo(), "工号不能为空");
			AssertUtil.checkNull(info.getPermissStart(), "权限期限开始时间不能为空");
			AssertUtil.checkNull(info.getLicensePlate(), "权限期限结束时间不能为空");

			Map<String,String> header = getHeaderToken();
			String reponseContent = HttpClientUtil.post(parkingProperties.getHost() + parkingProperties.getAddCarmanagementUrl(), JSON.toJSONString(info),header);
			log.info("调用添加车辆接口入参：{},出参:{}",JSON.toJSONString(info),JSON.toJSONString(reponseContent));
			ParkBaseRespBO<ParkBaseDetailRespBO> resp = JSON.parseObject(reponseContent, new TypeReference<ParkBaseRespBO<ParkBaseDetailRespBO>>() {} );
			return resp;
		} catch (Exception e) {
			log.info("调用添加车辆接口入参：{},e={}",JSON.toJSONString(info),e);
			return ParkBaseRespBO.fail();
		}
	}
	
	public static void main(String[] args) {
		Date d = DateUtil.endOfDay(new Date());
		System.out.println(d.toLocaleString());
	}

	/**
	 * @param dto
	 * @return
	 */
	public YardPageBO queryYard(QueryYardDTO dto) {
		
		try {
			AssertUtil.checkNull(dto, "参数不能为空");
			Map<String,String> header = getHeaderToken();

			String reponseContent = HttpClientUtil.post(parkingProperties.getHost() + parkingProperties.getQueryyardUrl(), JSON.toJSONString(dto),header);
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


	/**
	 * 查询车辆信息
	 * @param dto
	 * @return
	 */
	public ParkingCarQueryRespBO<ParkingCarInfoBO<ParkingYardBO>> queryCarInfo(ParkingCarQueryDTO dto) {
		try {
			log.info("查询车辆信息入参：{}",JSON.toJSONString(dto));
			Map<String,String> header = getHeaderToken();
			String reponseContent = HttpClientUtil.post(parkingProperties.getHost() + parkingProperties.getQueryyardUrl(), JSON.toJSONString(dto),header);
			log.info("查询车辆信息口入参：{},出参:{}",JSON.toJSONString(dto),JSON.toJSONString(reponseContent));
			ParkBaseRespBO<ParkingCarQueryRespBO<ParkingCarInfoBO<ParkingYardBO>>> resp = JSON.parseObject(reponseContent, new TypeReference<ParkBaseRespBO<ParkingCarQueryRespBO<ParkingCarInfoBO<ParkingYardBO>>>>() {} );
			if(ParkingRemoteCodeEnum.RESP_SUCCESS.getState().equals(resp.getResCode()) && resp.getResult() != null &&
					ParkingRemoteCodeEnum.BUS_CODE.getState().equals(resp.getResult().getCode())) {
				return resp.getResult();
			}
			return null;
		} catch (Exception e) {
			log.error("查询车辆信息出错：{}",e);
			return null;

		}


	}

	private Map<String,String> getHeaderToken() throws Exception{
		Map<String,String> header = new HashMap<>();
		String token = getToken();
		header.put("access-token", token);
		return header;
	}

	/**
	 * 获取token
	 * @return
	 * @throws Exception
	 */
	public String getToken() throws Exception {
			ParkingTokenBO tokenBo = redisUtil.get(RedisConstant.PARKING_TOKEN,ParkingTokenBO.class);
			if(tokenBo != null) {
				log.info("已存在token={},给token续期",tokenBo);
				redisUtil.expire(RedisConstant.PARKING_TOKEN, tokenBo.getExpires_in() - 10);
				return tokenBo.getAccess_token();
			}
			Map<String,String> obj = new HashMap<>();
			obj.put("grant_type", parkingProperties.getGrantType());
			obj.put("client_id", parkingProperties.getClientId());
			obj.put("client_secret", parkingProperties.getClientSecret());
			String resp = HttpClientUtil.postHttps(parkingProperties.getHost() + parkingProperties.getTokenUrl(), obj, null);
			log.info("获取token参数：{}，Result={}",JSON.toJSONString(obj),resp);
			ParkingTokenBO token = JSON.parseObject(resp, ParkingTokenBO.class);
			if (token != null) {
				redisUtil.put(RedisConstant.PARKING_TOKEN, token, token.getExpires_in() - 10);
				return token.getAccess_token();
			}
			throw new BusinessException("获取token失败");
	}

}
