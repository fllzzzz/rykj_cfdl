package com.cf.parking.services.integration;

import cn.hutool.core.date.DateUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import com.cf.parking.dao.po.UserProfilePO;
import com.cf.parking.facade.bo.*;
import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.facade.dto.*;
import com.cf.parking.services.constant.ParkingConstants;
import com.cf.parking.services.enums.ParkingRemoteCodeEnum;
import com.cf.parking.services.properties.ParkingProperties;
import com.cf.parking.services.service.UserProfileService;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.HttpClientUtils;
import com.cf.support.exception.BusinessException;
import com.cf.support.utils.DingAlarmUtils;
import com.cf.support.utils.HttpClientUtil;
import com.cf.support.utils.RedisUtil;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpResponseException;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.Map;

@Service
@Slf4j
public class ParkInvokeService {

	@Resource
	private ParkingProperties parkingProperties;
	
	@Resource
	private RedisUtil redisUtil;
	
	@Resource
	private UserProfileService userProfileService;

	/**
	 * 新增或修改车辆信息
	 * @param space
	 * @return
	 */
	public ParkBaseRespBO<ParkBaseDetailRespBO> replaceCarInfo(UserSpaceDTO space) {
		
		UserProfilePO user = userProfileService.selectUserProfileByNameAndJobNumber(null,space.getJobNumber());
		
		Carmanagement info = new Carmanagement();
		info.setLicensePlate(space.getPlateNo())
			.setJobNo(space.getJobNumber())
			.setParkIndexCode(space.getParkingLot().split(","))
			.setCarOwner(space.getName())
			.setOwnerPhone(user == null ? null : user.getMobile())
			.setPermissStart(DateUtil.format(DateUtil.beginOfDay(space.getStartDate()), ParkingConstants.FULL_DATE_FORMAT) )
			.setPermissEnd(DateUtil.format(DateUtil.endOfDay(space.getEndDate()), ParkingConstants.FULL_DATE_FORMAT));
		try {
			AssertUtil.checkNull(info, "参数不能为空");
			AssertUtil.checkNull(info.getLicensePlate(), "车牌号不能为空");
			AssertUtil.checkNull(info.getParkIndexCode(), "车库不能为空");
			AssertUtil.checkNull(info.getCarOwner(), "车主不能为空");
			AssertUtil.checkNull(info.getJobNo(), "工号不能为空");
			AssertUtil.checkNull(info.getPermissStart(), "权限期限开始时间不能为空");
			AssertUtil.checkNull(info.getLicensePlate(), "权限期限结束时间不能为空");

			Map<String,String> header = getHeaderToken();
			String reponseContent = HttpClientUtils.postJsonWithoutSSL(parkingProperties.getHost() + parkingProperties.getAddCarmanagementUrl(), JSON.toJSONString(info),header);
			log.info("调用添加车辆接口入参：{},出参:{}",JSON.toJSONString(info),JSON.toJSONString(reponseContent));
			ParkBaseRespBO<ParkBaseDetailRespBO> resp = JSON.parseObject(reponseContent, new TypeReference<ParkBaseRespBO<ParkBaseDetailRespBO>>() {} );
			if (ParkingRemoteCodeEnum.UNAUTH.getState().equals(resp.getResCode())){
				redisUtil.del(RedisConstant.PARKING_TOKEN);
				throw new BusinessException("会话过期,请重试");
			}
			return resp;
		} catch(HttpResponseException e) {
			log.info("调用添加车辆接口入参：{},调用失败e={}",JSON.toJSONString(info),e);
			if(e.getStatusCode() == HttpStatus.SC_UNAUTHORIZED) {
				redisUtil.del(RedisConstant.PARKING_TOKEN);
			}
			DingAlarmUtils.alarmException("调用添加车辆接口失败"+ ExceptionUtils.getStackTrace(e));
			return ParkBaseRespBO.fail();
		} catch(BusinessException e) {
			return ParkBaseRespBO.fail(e.getMsg());
		} catch (Exception e) {
			log.error("调用添加车辆接口入参：{},e={}",JSON.toJSONString(info),e);
			DingAlarmUtils.alarmException("调用添加车辆接口失败"+ ExceptionUtils.getStackTrace(e));
			return ParkBaseRespBO.fail();
		}
	}
	
	/**
	 * @param dto
	 * @return
	 */
	public YardPageBO queryYard(QueryYardDTO dto) {
		
		try {
			AssertUtil.checkNull(dto, "参数不能为空");
			Map<String,String> header = getHeaderToken();

			String reponseContent = HttpClientUtils.postJsonWithoutSSL(parkingProperties.getHost() + parkingProperties.getQueryyardUrl(), JSON.toJSONString(dto),header);
			log.info("调用查询车库接口入参：{},出参:{}",JSON.toJSONString(dto),JSON.toJSONString(reponseContent));
			ParkBaseRespBO<YardPageBO> resp = JSON.parseObject(reponseContent, new TypeReference<ParkBaseRespBO<YardPageBO>>() {} );
			if(ParkingRemoteCodeEnum.RESP_SUCCESS.getState().equals(resp.getResCode()) && resp.getResult() != null && 
					ParkingRemoteCodeEnum.BUS_CODE.getState().equals(resp.getResult().getCode())) {
				YardPageBO result = resp.getResult();
				result.setData(JSON.parseArray(JSON.toJSONString(result.getData()), YardDetailBO.class));
				return result;
			} else if (ParkingRemoteCodeEnum.UNAUTH.getState().equals(resp.getResCode())){
				redisUtil.del(RedisConstant.PARKING_TOKEN);
				throw new BusinessException("会话过期,请重试");
			}
			return null;
		} catch(HttpResponseException e) {
			log.error("调用查询车库接口入参：{},调用失败e={}",JSON.toJSONString(dto),e);
			if(e.getStatusCode() == HttpStatus.SC_UNAUTHORIZED) {
				redisUtil.del(RedisConstant.PARKING_TOKEN);
			}
			DingAlarmUtils.alarmException("调用查询车库接口失败"+ ExceptionUtils.getStackTrace(e));

			return null;
			
		} catch (Exception e) {
			DingAlarmUtils.alarmException("调用查询车库接口失败"+ ExceptionUtils.getStackTrace(e));

			log.error("调用查询车库接口入参：{},e:{}",JSON.toJSONString(dto),e);
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
			String reponseContent = HttpClientUtils.postJsonWithoutSSL(parkingProperties.getHost() + parkingProperties.getQueryCarUrl(), JSON.toJSONString(dto),header);
			log.info("查询车辆信息口入参：{},出参:{}",JSON.toJSONString(dto),JSON.toJSONString(reponseContent));
			ParkBaseRespBO<ParkingCarQueryRespBO<ParkingCarInfoBO<ParkingYardBO>>> resp = JSON.parseObject(reponseContent, new TypeReference<ParkBaseRespBO<ParkingCarQueryRespBO<ParkingCarInfoBO<ParkingYardBO>>>>() {} );
			if(ParkingRemoteCodeEnum.RESP_SUCCESS.getState().equals(resp.getResCode()) && resp.getResult() != null &&
					ParkingRemoteCodeEnum.BUS_CODE.getState().equals(resp.getResult().getCode())) {
				return resp.getResult();
			} else if (ParkingRemoteCodeEnum.UNAUTH.getState().equals(resp.getResCode())){
				redisUtil.del(RedisConstant.PARKING_TOKEN);
				throw new BusinessException("会话过期,请重试");
			}
			return null;
		} catch(HttpResponseException e) {
			log.error("调用查询车辆接口入参：{},调用失败e={}",JSON.toJSONString(dto),e);
			if(e.getStatusCode() == HttpStatus.SC_UNAUTHORIZED) {
				redisUtil.del(RedisConstant.PARKING_TOKEN);
			}
			DingAlarmUtils.alarmException("调用查询车辆接口失败"+ ExceptionUtils.getStackTrace(e));

			return null;
			
		} catch (Exception e) {
			log.error("查询车辆信息出错",e);
			DingAlarmUtils.alarmException("调用查询车辆接口失败"+ ExceptionUtils.getStackTrace(e));

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
	 * 删除车辆
	 * @param dto
	 * @return
	 */
	public ParkBaseRespBO<ParkBaseDetailRespBO> deleteCarInfo(ParkingDeleteCarDTO dto) {
		try {
			AssertUtil.checkNull(dto, "参数不能为空");
			Map<String,String> header = getHeaderToken();
			String reponseContent = HttpClientUtils.postJsonWithoutSSL(parkingProperties.getHost() + parkingProperties.getDeleteCarUrl(), JSON.toJSONString(dto),header);
			log.info("调用删除车辆接口入参：{},出参:{}",JSON.toJSONString(dto),JSON.toJSONString(reponseContent));
			ParkBaseRespBO<ParkBaseDetailRespBO> resp = JSON.parseObject(reponseContent, new TypeReference<ParkBaseRespBO<ParkBaseDetailRespBO>>() {} );
			if (ParkingRemoteCodeEnum.UNAUTH.getState().equals(resp.getResCode())){
				redisUtil.del(RedisConstant.PARKING_TOKEN);
				throw new BusinessException("会话过期,请重试");
			}
			return resp;
		} catch(HttpResponseException e) {
			log.error("调用删除车辆接口入参：{},调用失败e={}",JSON.toJSONString(dto),e);
			if(e.getStatusCode() == HttpStatus.SC_UNAUTHORIZED) {
				redisUtil.del(RedisConstant.PARKING_TOKEN);
			}
			DingAlarmUtils.alarmException("调用删除车辆接口失败"+ ExceptionUtils.getStackTrace(e));

			return ParkBaseRespBO.fail();
			
		} catch (Exception e) {
			log.error("调用删除辆接口入参：{},e={}",JSON.toJSONString(dto),e);
			DingAlarmUtils.alarmException("调用删除车辆接口失败"+ ExceptionUtils.getStackTrace(e));

			return ParkBaseRespBO.fail();
		}
	}
	
	/**
	 * 获取token
	 * @return
	 * @throws Exception
	 */
	public String getToken() throws Exception {
			ParkingTokenBO tokenBo = redisUtil.get(RedisConstant.PARKING_TOKEN,ParkingTokenBO.class);
			if(tokenBo != null) {
				log.info("已存在token={}",tokenBo);
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
