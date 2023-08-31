package com.cf.parking.services.integration;

import com.cf.parking.facade.dto.ScheduleDataDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.Map;

/**
 * @author whx
 * @date 2023/3/27
 */
@FeignClient(url = "${gateway.url}" + "/gaiawork", name = "GatewayGaiaWorkFeign", configuration = GaiaWorkClientConfiguration.class)
public interface GatewayGaiaWorkFeign {

	/**
	 * 获取盖亚token
	 * 接口文档: 查询原始打卡数据.pdf
	 *
	 * @param grantType
	 * @param corpId
	 * @param clientSecret
	 * @return
	 */
	@RequestMapping(headers = {"content-type=application/x-www-form-urlencoded"}, method = RequestMethod.POST, value = "/identity/api/v1/oauth")
	Map<String, Object> getAccessToken(@RequestParam("grant_type") String grantType, @RequestParam("corp_id") String corpId, @RequestParam("client_secret") String clientSecret);

	/**
	 * 获取盖亚排班数据
	 * A1人员排班标准接口.pdf
	 *
	 * @param authorization
	 * @param scheduleDataDTO
	 * @return
	 */
	@RequestMapping(headers = {"content-type=application/json"}, method = RequestMethod.POST, value = "/wfm4integration-wfm4appapi/api/gaiaStandard/attendance/getScheduleData/cfmoto")
	Map<String, Object> getGaiaWorkForceAttendance(@RequestHeader("Authorization") String authorization, ScheduleDataDTO scheduleDataDTO);
}
