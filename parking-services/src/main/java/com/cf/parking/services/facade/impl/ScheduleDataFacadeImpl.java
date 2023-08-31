package com.cf.parking.services.facade.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateUtil;
import com.cf.parking.dao.po.ScheduleDataPO;
import com.cf.parking.facade.bo.ScheduleDateBO;
import com.cf.parking.facade.dto.ScheduleDataDTO;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.parking.facade.facade.ScheduleDataFacade;
import com.cf.parking.services.integration.GatewayGaiaWorkFeign;
import com.cf.parking.services.service.ScheduleDataService;
import com.cf.support.exception.BusinessException;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author whx
 * @date 2023/3/27
 */
@Service
public class ScheduleDataFacadeImpl implements ScheduleDataFacade {
	@Resource
	private GatewayGaiaWorkFeign gatewayGaiaworkFeign;
	@Resource
	private ScheduleDataService scheduleDataService;

	@Override
	public ScheduleDateBO getScheduleDate() {
		// 获取当前季度日期
		Date startDate = this.getSeasonStartDate(new Date());
		Date endDate = this.getSeasonEndDate(new Date());
		List<ScheduleDataPO> scheduleDataPOS = scheduleDataService.getScheduleDate(startDate, endDate);
		List<String> scheduleDateList = scheduleDataPOS.stream().map(o -> DateFormatUtils.format(o.getShiftDate(), "yyyy-MM-dd")).collect(Collectors.toList());
		return new ScheduleDateBO().setShiftDateList(scheduleDateList);
	}

	private String getGaiaToken() {
		//调接口
		Map<String, Object> accessToken = gatewayGaiaworkFeign.getAccessToken("client_credentials", "cfmoto", "acfdf03b20424fb0af733e38c791526f");
		if (Integer.valueOf((Integer) accessToken.get("code")) != 200) {
			throw new BusinessException(BizResultCodeEnum.TOKEN_BUILD_ERROR.getMsg());
		}
		return String.valueOf(accessToken.get("data"));
	}

	@Override
	@Transactional(rollbackFor = Exception.class)
	public void getGaiaAttendance() {
		// 获取当前季度日期
		Date startDate = this.getSeasonStartDate(new Date());
		Date endDate = this.getSeasonEndDate(new Date());
		// 是否已获取当季数据
		if (scheduleDataService.countSeasonData(startDate, endDate) != 0L) {
			return;
		}
		// 获取token
		String accessToken = this.getGaiaToken();
		String authorization = "Bearer " + accessToken;
		ScheduleDataDTO scheduleDataDTO = new ScheduleDataDTO().setEmpId("CFDL14966")
				.setStartDate(DateFormatUtils.format(startDate, "yyyy-MM-dd")).setEndDate(DateFormatUtils.format(endDate, "yyyy-MM-dd"));
		//调接口
		Map<String, Object> workForceAttendance = gatewayGaiaworkFeign.getGaiaWorkForceAttendance(authorization, scheduleDataDTO);
		if (!"GWDA_00000".equals(String.valueOf(workForceAttendance.get("errorCode")))) {
			throw new BusinessException(String.valueOf(workForceAttendance.get("errorMsg")));
		}
		List<ScheduleDataPO> scheduleDataPOS = new ArrayList<>();
		List<Map<String, String>> dataList = (List<Map<String, String>>) workForceAttendance.get("data");
		dataList.forEach(data -> {
			// 剔除未排班数据
			if (StringUtils.isBlank(data.get("hours"))) {
				return;
			}
			ScheduleDataPO scheduleDataPO = BeanUtil.toBean(data, ScheduleDataPO.class);
			Date shiftDate = DateUtil.parse(data.get("shiftDate"), "yyyy-MM-dd");
			scheduleDataPO.setShiftDate(shiftDate);
			scheduleDataPOS.add(scheduleDataPO);
		});
		scheduleDataService.saveBatch(scheduleDataPOS);
	}

	/**
	 * 计算某日期所在季度结束日期
	 * 季度划分：1、2、3， 4、5、6， 7、8、9， 10、11、12
	 */
	private Date getSeasonEndDate(Date date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		int month = calendar.get(Calendar.MONTH);
		calendar.set(Calendar.MONTH, (month + 3) / 3 * 3);
		calendar.set(Calendar.DATE, 1);
		return new Date(calendar.getTime().getTime() - 24 * 60 * 60 * 1000);
	}

	/**
	 * 计算某日期所在季度开始日期
	 * 季度划分：1、2、3， 4、5、6， 7、8、9， 10、11、12
	 */
	private Date getSeasonStartDate(Date date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		int month = calendar.get(Calendar.MONTH);
		calendar.set(Calendar.MONTH, month / 3 * 3);
		calendar.set(Calendar.DATE, 1);
		return calendar.getTime();
	}
}
