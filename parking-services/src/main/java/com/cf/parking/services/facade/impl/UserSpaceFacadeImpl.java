package com.cf.parking.services.facade.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.cf.parking.dao.mapper.UserSpaceMapper;
import com.cf.parking.dao.po.UserSpacePO;
import com.cf.parking.facade.bo.UserSpaceBO;
import com.cf.parking.facade.constant.FeignUrlConstant;
import com.cf.parking.facade.dto.HikvisionResult;
import com.cf.parking.facade.dto.UserSpaceDTO;
import com.cf.parking.facade.dto.UserSpacePageDTO;
import com.cf.parking.facade.dto.UserSpaceSyncDTO;
import com.cf.parking.facade.facade.UserSpaceFacade;
import com.cf.parking.services.integration.GatewayHikvisionFeign;
import com.cf.parking.services.service.UserSpaceService;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.cf.parking.facade.constant.ParkingSysCodeConstant.parkingSysCodeMap;

/**
 * @author lpy
 * @date 2023/3/29
 */
@Slf4j
@Service
public class UserSpaceFacadeImpl implements UserSpaceFacade {
	private static final Long pageNo = 1L;
	private static final Long PAGE_SIZE = 1000L;
	@Resource
	private UserSpaceService userSpaceService;
	@Resource
	private GatewayHikvisionFeign gatewayHikvisionFeign;
	@Resource
	private UserSpaceMapper userSpaceMapper;


	@Override
	public PageResponse<UserSpaceBO> getUserSpacePage(UserSpacePageDTO param) {
		IPage userSpacePage = userSpaceService.getUserSpacePage(param);
		if (userSpacePage.getTotal() == 0) {
			return PageUtils.emptyResponseList(userSpacePage);
		}
		List<UserSpacePO> spacePageRecords = userSpacePage.getRecords();
		Map<Long, UserSpacePO> userSpacePOMap = spacePageRecords.stream().collect(Collectors.toMap(UserSpacePO::getUserSpaceId, userSpacePO -> userSpacePO));
		List<UserSpaceBO> userSpaceBOS = BeanConvertorUtils.copyList(spacePageRecords, UserSpaceBO.class);
		userSpaceBOS.forEach(userSpaceBO -> {
			UserSpacePO userSpacePO = userSpacePOMap.get(userSpaceBO.getUserSpaceId());
			userSpaceBO.setStartDate(DateFormatUtils.format(userSpacePO.getStartDate(), "yyyy/MM/dd"))
					.setEndDate(DateFormatUtils.format(userSpacePO.getEndDate(), "yyyy/MM/dd"));
		});
		return PageUtils.toResponseList(userSpacePage, userSpaceBOS);
	}

	@Override
	public List<UserSpaceBO> getUserSpaceList(UserSpacePageDTO param) {
		List<UserSpacePO> userSpacePOS = userSpaceService.getUserSpaceList(param);
		if (CollectionUtils.isEmpty(userSpacePOS)) {
			return new ArrayList<>();
		}
		Map<Long, UserSpacePO> userSpacePOMap = userSpacePOS.stream().collect(Collectors.toMap(UserSpacePO::getUserSpaceId, userSpacePO -> userSpacePO));
		List<UserSpaceBO> userSpaceBOS = BeanConvertorUtils.copyList(userSpacePOS, UserSpaceBO.class);
		userSpaceBOS.forEach(userSpaceBO -> {
			UserSpacePO userSpacePO = userSpacePOMap.get(userSpaceBO.getUserSpaceId());
			userSpaceBO.setStartDate(DateFormatUtils.format(userSpacePO.getStartDate(), "yyyy/MM/dd"))
					.setEndDate(DateFormatUtils.format(userSpacePO.getEndDate(), "yyyy/MM/dd")
					);
		});
		return userSpaceBOS;
	}

	@Override
	@Transactional(rollbackFor = Exception.class)
	public void syncUserSpaceData() throws InterruptedException {
		// 删除表中数据
		userSpaceMapper.deleteAll();

		LinkedList<UserSpacePO> resultList = new LinkedList<>();
		// 遍历车库码 (不按车库查询，查出的数据很少，只有原来的一半)
		for (Map.Entry<String, String> entry : parkingSysCodeMap.entrySet()) {
			String value = entry.getValue();
			boolean flag = false;
			// 当前页
			Long currentPage = 1L;
			// 总页数，初始设置为2， 调用一次接口后会被覆盖
			Long totalPageNo = 2L;
			while (currentPage <= totalPageNo) {
				UserSpaceSyncDTO userSpaceSyncDTO = new UserSpaceSyncDTO()
						.setPageNo(currentPage).setPageSize(PAGE_SIZE).setParkSyscodes(value);
				log.info("getUserSpace:获取海康车辆包期信息：headers:{}", JSON.toJSONString(userSpaceSyncDTO));
				HikvisionResult<PageResponse<UserSpaceDTO>> dataRes = gatewayHikvisionFeign.userSpaceDataSync(FeignUrlConstant.CAR_CHARGE_URL, userSpaceSyncDTO);
				log.info("getUserSpace:获取海康车辆包期信息：response:{}", JSON.toJSONString(dataRes));
				PageResponse<UserSpaceDTO> data = dataRes.getData();
				Long total = data.getTotal();
				// 分批处理数据并保存
				List<UserSpacePO> userSpacePOS = userSpaceService.getUserSpaceList(data);
				resultList.addAll(userSpacePOS);

				// 只有第一次调用接口才进入
				if (!flag) {
					// 分批处理数据并保存
					totalPageNo = total / PAGE_SIZE + 1;
					flag = true;
					currentPage += 1;
					continue;
				}
				currentPage += 1;
				// 防止触发接口风控
				Thread.sleep(3000);
			}
			Thread.sleep(3000);
		}
		userSpaceService.mergeParkingLots(resultList);
	}


}
