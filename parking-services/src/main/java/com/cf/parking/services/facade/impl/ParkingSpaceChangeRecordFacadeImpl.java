package com.cf.parking.services.facade.impl;

import java.util.*;
import java.util.stream.Collectors;

import javax.annotation.Resource;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.ParkingSpaceChangeRecordPOMapper;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.dao.po.ParkingSpaceChangeRecordPO;
import com.cf.parking.dao.po.UserInfoPO;
import com.cf.parking.dao.po.UserPO;
import com.cf.parking.dao.po.UserSpacePO;
import com.cf.parking.facade.bo.ParkingSpaceChangeRecordBO;
import com.cf.parking.facade.dto.ParkingSpaceChangeApplyDTO;
import com.cf.parking.facade.dto.ParkingSpaceChangeRecordDTO;
import com.cf.parking.facade.facade.ParkingSpaceChangeRecordFacade;
import com.cf.parking.services.constant.ParkingConstants;
import com.cf.parking.services.enums.ChangeRecordStateEnum;
import com.cf.parking.services.enums.UserSpaceTypeEnum;
import com.cf.parking.services.service.ParkingLotService;
import com.cf.parking.services.service.ParkingSpaceChangeRecordService;
import com.cf.parking.services.service.UserProfileService;
import com.cf.parking.services.service.UserService;
import com.cf.parking.services.service.UserSpaceService;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;

import cn.hutool.core.date.DateUtil;
import lombok.extern.slf4j.Slf4j;



@Slf4j
@Service
public class ParkingSpaceChangeRecordFacadeImpl implements ParkingSpaceChangeRecordFacade {

	@Resource
	private ParkingSpaceChangeRecordPOMapper changeRecordPOMapper;
	
	
	@Resource
	private ParkingLotService parkingLotService;
	
	@Resource
	private UserSpaceService userSpaceService;
	
	@Resource
	private UserProfileService userProfileService;
	
	@Resource
	private IdWorker idWorker;
	
	@Resource
	private UserService userService;
	
	@Resource
	private ParkingSpaceChangeRecordService parkingSpaceChangeRecordService;
	
	
	
	
	@Override
	public long getParkingSpaceChangeRecoudCount(ParkingSpaceChangeRecordDTO dto) {
		return changeRecordPOMapper.selectCount(new LambdaQueryWrapper<ParkingSpaceChangeRecordPO>()
        		.eq(ObjectUtils.isNotEmpty(dto.getState()), ParkingSpaceChangeRecordPO::getState, dto.getState())
                .like(ObjectUtils.isNotEmpty(dto.getAcceptUserName()), ParkingSpaceChangeRecordPO::getAcceptUserName, dto.getAcceptUserName())
                .like(ObjectUtils.isNotEmpty(dto.getUserName()), ParkingSpaceChangeRecordPO::getUserName, dto.getUserName())
                .and(ObjectUtils.isNotEmpty(dto.getUserId()), 
                		i -> i.eq(ObjectUtils.isNotEmpty(dto.getUserId()), ParkingSpaceChangeRecordPO::getAcceptUserId, dto.getUserId())
                			.or()
                			.eq(ObjectUtils.isNotEmpty(dto.getUserId()), ParkingSpaceChangeRecordPO::getUserId, dto.getUserId()))
                );
		
	}
	
	
	@Override
	public PageResponse<ParkingSpaceChangeRecordBO> getParkingSpaceChangeRecordList(ParkingSpaceChangeRecordDTO dto) {
		log.info("查询互换记录params:{}",JSON.toJSONString(dto));
		Page<ParkingSpaceChangeRecordPO> page = PageUtils.toPage(dto);

		Page<ParkingSpaceChangeRecordPO> poPage = changeRecordPOMapper.selectPage(page,
        		new LambdaQueryWrapper<ParkingSpaceChangeRecordPO>()
        		.eq(ObjectUtils.isNotEmpty(dto.getState()), ParkingSpaceChangeRecordPO::getState, dto.getState())
				.eq(StringUtils.isNotBlank(dto.getParkingLotCode()),ParkingSpaceChangeRecordPO::getParkingCode,dto.getParkingLotCode())
				.ge(ObjectUtils.isNotEmpty(dto.getApplyStartDate()),ParkingSpaceChangeRecordPO::getCreateTm,dto.getApplyStartDate())
				.le(ObjectUtils.isNotEmpty(dto.getApplyEndDate()),ParkingSpaceChangeRecordPO::getCreateTm,null == dto.getApplyEndDate() ? null : DateUtil.endOfDay(dto.getApplyEndDate()))
				.and(ObjectUtils.isNotEmpty(dto.getUserName()),
						i -> i.like(ObjectUtils.isNotEmpty(dto.getUserName()),ParkingSpaceChangeRecordPO::getUserName,dto.getUserName())
								.or()
								.like(ObjectUtils.isNotEmpty(dto.getUserName()),ParkingSpaceChangeRecordPO::getAcceptUserName,dto.getUserName()))
                .orderByDesc(ParkingSpaceChangeRecordPO::getCreateTm));

        List<ParkingSpaceChangeRecordBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), ParkingSpaceChangeRecordBO.class);
		Map<String,String> parkingMap = new HashMap<>();
        boList.forEach(item -> setParkingLotRegionByCode(item,parkingMap));
		return PageUtils.toResponseList(page,boList);
	}


	private void setParkingLotRegionByCode(ParkingSpaceChangeRecordBO bo, Map<String, String> parkingMap) {
		ParkingLotPO parkingLotPO = null;
		String parkingName = parkingMap.get(bo.getParkingCode());
		if (StringUtils.isEmpty(parkingName)){
			parkingLotPO = parkingLotService.selectParkingLotByCode(bo.getParkingCode());
			if (parkingLotPO != null) {
				parkingName = parkingLotPO.getRegion();
				parkingMap.put(bo.getParkingCode(), parkingName);
			}
		}
		bo.setParkingName(parkingName);
		
		String acceptParkingName = parkingMap.get(bo.getAcceptParkingCode());
		if (StringUtils.isEmpty(acceptParkingName)){
			parkingLotPO = parkingLotService.selectParkingLotByCode(bo.getAcceptParkingCode());
			if (parkingLotPO != null) {
				acceptParkingName = parkingLotPO.getRegion();
				parkingMap.put(bo.getAcceptParkingCode(), acceptParkingName);
			}
		}
		bo.setAcceptParkingName(acceptParkingName);
	}


	@Override
	public void applyChange(ParkingSpaceChangeApplyDTO dto) {
		log.info("进入车位互换：param={}",JSON.toJSONString(dto));
		//查询是否有申请状态的申请记录
		ParkingSpaceChangeRecordDTO recordDto = new ParkingSpaceChangeRecordDTO().setUserId(dto.getUserId()).setState(ChangeRecordStateEnum.APPLY.getState());
		long count = getParkingSpaceChangeRecoudCount(recordDto);
		AssertUtil.checkTrue(count == 0, "您已申请过或已收到别人的申请,请先处理完毕后再操作");
		//判断申请人车位是否存在
		long num = userSpaceService.count(new LambdaQueryWrapper<UserSpacePO>()
				.eq(UserSpacePO::getJobNumber, dto.getJobNumber())
				.eq(UserSpacePO::getParkingLot, dto.getParkingCode())
				.eq(UserSpacePO::getStartDate, DateUtil.format(DateUtil.beginOfDay(dto.getValidStartDate()), ParkingConstants.SHORT_DATE_FORMAT) )
				.eq(UserSpacePO::getEndDate, DateUtil.format(DateUtil.beginOfDay(dto.getValidEndDate()), ParkingConstants.SHORT_DATE_FORMAT))
			);
		AssertUtil.checkTrue(num > 0, "您申请交换的车库不存在");
		//查询交换人车位
		List<UserSpacePO> spaceList = userSpaceService.querySpaceGroupByExpireDate(dto.getAcceptJobNumber(), UserSpaceTypeEnum.LOTTERY.getState());		
		AssertUtil.checkNull(spaceList, "所选人员无车位，无法交换");
		//过滤出车库不一致、
		spaceList = spaceList.stream().filter(space -> !space.getParkingLot().equals(dto.getParkingCode())).collect(Collectors.toList());
		AssertUtil.checkNull(spaceList, "所选人员和您同车库，无法交换");
		
		UserInfoPO user = userProfileService.getUserInfoByUserId(dto.getUserId());
		AssertUtil.checkNull(user, "用户不存在");
		
		UserPO acceptor = userService.selectByOpenId(dto.getAcceptJobNumber());		
		AssertUtil.checkNull(user, "交换人不存在");
		
		//过滤出时间有交集的，假如连续2期，AB人员有几种中签情况(必须中签才能交换)，可以看出来可以交换的情况开始日期和结束日期肯定有个相同的，目前限定结束日期必须相同
		/**
		 *    人员     期数      人员		期数		可否交换
		 *    A		   1		B		1		  可
		 *    A		   1		B		2		  否
		 * 	  A		   2		B		1		  否
		 * 	  A		   2		B		2		  可
		 *    A		  1,2		B		1		  可，第一期就可交换,第二期出来后不可交换
		 *    A		  1,2		B		2		  可，第二期才可交换
		 *    A		  1,2		B		1,2		  可，一直都可交换
		 *    A		  1			B		1,2		  可，第一期就可交换,第二期出来后不可交换
		 *    A		  2			B		1,2		  可，第二期才可交换
		 */
		//这是针对那种连续2期中签，对方是第二期中签，这种场景下意味着第二期已经开奖，那么第一期是不允许交换的
		spaceList = spaceList.stream().filter(space ->  DateUtil.beginOfDay(space.getEndDate()).compareTo(DateUtil.beginOfDay(dto.getValidEndDate())) == 0).collect(Collectors.toList());
		AssertUtil.checkNull(spaceList, "所选人员车库有效期和您所选的有效期不匹配，无法交换");
		//走到这里，代表交换人有可供交换的车库，要进行判断是否是第二期才可交换的情况，是的话要判断当前日期是否大于第二期的开始日期，否则不能交换
		boolean changeAble = false;
		for(UserSpacePO space : spaceList) {
			//开始日期也相同，可以交换
			if ( DateUtil.beginOfDay(space.getStartDate()).compareTo(DateUtil.beginOfDay(dto.getValidStartDate())) == 0 ) {
				changeAble = true;
				//存在可以交换的车库，就不用继续判断了
				break;
			}
			
			//开始日期不相同，则获取这两个车库的较大的开始日期
			Date date = dto.getValidStartDate();
			date = date.compareTo(space.getStartDate()) > 0 ? date : space.getStartDate();
			if(DateUtil.beginOfDay(new Date()).compareTo(date) >= 0 ) {
				//当前日期大于最大的开始日期，意味着上一期已经全部结束，那么是可以交换的
				changeAble = true;
			}
		}
		AssertUtil.checkTrue(changeAble, "请等到下一期车库生效后在进行交换");
		ParkingSpaceChangeRecordPO record = new ParkingSpaceChangeRecordPO();
		BeanUtils.copyProperties(dto, record);
		
		record.setUserName(user.getName())
			  .setState(ChangeRecordStateEnum.APPLY.getState())
			  .setCreateTm(new Date())
			  .setUpdateTm(new Date())
			  .setId(idWorker.nextId())
			  .setAcceptUserId(acceptor.getUserId())
			  ;
		changeRecordPOMapper.insert(record);
	
	}


	@Transactional(rollbackFor = Exception.class)
	@Override
	public void deal(ParkingSpaceChangeApplyDTO param) {
		ChangeRecordStateEnum state = ChangeRecordStateEnum.getStateEnum(param.getState());
		AssertUtil.checkNull(state, "状态非法");
		ParkingSpaceChangeRecordPO record = changeRecordPOMapper.selectById(param.getId());
		AssertUtil.checkNull(record, "数据不存在");
		switch(state) {
			case AGREE:
				parkingSpaceChangeRecordService.agreeChange(record,param);
				break;
			case REJECT:
				record.setState(ChangeRecordStateEnum.REJECT.getState())
				  .setUpdateTm(new Date());
				changeRecordPOMapper.updateById(record);
				break;
			case CANCEL:
				record.setState(ChangeRecordStateEnum.CANCEL.getState())
					  .setUpdateTm(new Date());
				changeRecordPOMapper.updateById(record);
				break;
			default :
				break;
		}
	}


}
