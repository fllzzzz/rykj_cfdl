package com.cf.parking.services.service;

import java.util.Date;

import javax.annotation.Resource;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.ParkingSpaceChangeRecordPOMapper;
import com.cf.parking.dao.po.ParkingSpaceChangeRecordPO;
import com.cf.parking.dao.po.UserInfoPO;
import com.cf.parking.dao.po.UserSpacePO;
import com.cf.parking.facade.dto.ParkingSpaceChangeApplyDTO;
import com.cf.parking.services.constant.ParkingConstants;
import com.cf.parking.services.enums.ChangeRecordStateEnum;
import com.cf.parking.services.enums.UserSpaceStateEnum;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.exception.BusinessException;
import cn.hutool.core.date.DateUtil;
import lombok.extern.slf4j.Slf4j;



@Slf4j
@Service
public class ParkingSpaceChangeRecordService extends ServiceImpl<ParkingSpaceChangeRecordPOMapper, ParkingSpaceChangeRecordPO> implements IService<ParkingSpaceChangeRecordPO>{
	
	@Resource
	private ParkingSpaceChangeRecordPOMapper spaceChangeRecordPOMapper;
	
	@Resource
	private UserSpaceService userSpaceService;
	
	@Resource
	private UserProfileService userProfileService;
	
	/**
	 * 车位交换之同意交换
	 * @param record 交换申请对象
	 * @param param 同意交换的入参
	 */
	@Transactional(rollbackFor = Exception.class)
	public void agreeChange(ParkingSpaceChangeRecordPO record, ParkingSpaceChangeApplyDTO param) {
		log.info("同意车位交换：record={},param ={}",JSON.toJSONString(record),JSON.toJSONString(param));
		//检查传过来的车库和有效期是否存在
		long num = userSpaceService.count(new LambdaQueryWrapper<UserSpacePO>()
					.eq(UserSpacePO::getJobNumber, param.getAcceptJobNumber())
					.eq(UserSpacePO::getParkingLot, param.getAcceptParkingCode())
					.eq(UserSpacePO::getStartDate, DateUtil.format(DateUtil.beginOfDay(param.getValidStartDate()), ParkingConstants.SHORT_DATE_FORMAT) )
					.eq(UserSpacePO::getEndDate, DateUtil.format(DateUtil.beginOfDay(param.getValidEndDate()), ParkingConstants.SHORT_DATE_FORMAT))
				);
		AssertUtil.checkTrue(num > 0, "您同意交换的车库不存在");
		AssertUtil.checkTrue(!record.getParkingCode().equals(param.getAcceptParkingCode()), "您同意交换的车库不能和申请的车库相同");
		AssertUtil.checkTrue(DateUtil.beginOfDay(param.getValidEndDate()).compareTo(DateUtil.beginOfDay(record.getValidEndDate())) == 0, "同意的车库有效期截止日期必须相同");
		
		//如果起始日期不相同，则判断当前日期是否大于两者中最大的起始日期
		if (DateUtil.beginOfDay(param.getValidStartDate()).compareTo(DateUtil.beginOfDay(record.getValidStartDate())) != 0) {
			Date date = param.getValidStartDate().compareTo(record.getValidStartDate()) > 0 ? param.getValidStartDate() : record.getValidStartDate();
			if(DateUtil.beginOfDay(new Date()).compareTo(date) < 0 ) {
				//当前日期大于最大的开始日期，意味着上一期已经全部结束，那么是可以交换的
				throw new BusinessException("请等到下一期车库生效后在进行交换");
			}
			//把申请记录的起始日期改成两者中的最大者
			record.setValidStartDate(DateUtil.beginOfDay(date));
		}
		record.setAcceptParkingCode(param.getAcceptParkingCode());
		record.setUpdateTm(new Date());
		record.setState(ChangeRecordStateEnum.AGREE.getState());
		spaceChangeRecordPOMapper.updateById(record);
		//更新双方的车库、有效期信息
		UserInfoPO user = userProfileService.getUserInfoByUserId(record.getUserId());
		AssertUtil.checkNull(user, "用户不存在");
		
		UserSpacePO space = new UserSpacePO();
		//更新申请人的车库信息
		space.setScheduleDate("")
			.setState(UserSpaceStateEnum.UNSYNC.getState())
			.setParkingLot(record.getAcceptParkingCode())
			.setStartDate(record.getValidStartDate())
			.setEndDate(record.getValidEndDate());
		userSpaceService.update(space, new LambdaUpdateWrapper<UserSpacePO>()
									.eq(UserSpacePO::getJobNumber, user.getJobNumber())
									.eq(UserSpacePO::getParkingLot, record.getParkingCode())
									.eq(UserSpacePO::getEndDate, DateUtil.format(record.getValidEndDate(), ParkingConstants.SHORT_DATE_FORMAT))
		);
		//更新交换人的车库信息
		space.setParkingLot(record.getParkingCode());
		userSpaceService.update(space, new LambdaUpdateWrapper<UserSpacePO>()
				.eq(UserSpacePO::getJobNumber, param.getAcceptJobNumber())
				.eq(UserSpacePO::getParkingLot, param.getAcceptParkingCode())
				.eq(UserSpacePO::getEndDate, DateUtil.format(record.getValidEndDate(), ParkingConstants.SHORT_DATE_FORMAT))
		);
	}

}
