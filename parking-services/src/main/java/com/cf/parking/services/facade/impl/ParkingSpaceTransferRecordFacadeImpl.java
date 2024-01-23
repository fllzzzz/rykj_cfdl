package com.cf.parking.services.facade.impl;

import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import javax.annotation.Resource;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.cf.parking.dao.mapper.ParkingSpaceTransferRecordMapper;
import com.cf.parking.dao.po.*;
import com.cf.parking.facade.facade.ParkingSpaceTransferRecordFacade;
import com.cf.parking.services.enums.UserSpaceTypeEnum;
import com.cf.parking.services.service.*;
import com.cf.parking.services.utils.AssertUtil;
import cn.hutool.core.date.DateUtil;
import lombok.extern.slf4j.Slf4j;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.facade.bo.ParkingSpaceTransferRecordBO;
import com.cf.parking.facade.dto.ParkingSpaceTransferRecordDTO;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

/**
 * 车位转赠记录Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@Service
public class ParkingSpaceTransferRecordFacadeImpl implements ParkingSpaceTransferRecordFacade
{
    @Autowired
    private ParkingSpaceTransferRecordMapper parkingSpaceTransferRecordMapper;

    @Resource
    private UserSpaceService userSpaceService;
    
    @Resource
    private UserVerifyService userVerifyService;
    
    @Resource
    private LotteryBlackListService lotteryBlackListService;
    
    @Resource
    private LotteryDealService lotteryDealService;

	@Resource
	private ParkingLotService parkingLotService;
	
	@Resource
	private UserProfileService userProfileService;
    
	
	
	
	@Transactional(rollbackFor = Exception.class)
	@Override
	public void transfer(String outJobNum, String inJobNum) {
		log.info("转让开始，转让人：{},受让人：{}",outJobNum,inJobNum);;
		List<UserSpacePO> spaceList = userSpaceService.querySpaceGroupByExpireDate(outJobNum,UserSpaceTypeEnum.LOTTERY.getState());

		AssertUtil.checkTrue(!CollectionUtils.isEmpty(spaceList), "您无车位，无法进行转赠");
		//移除当天到期的车位信息
		Iterator<UserSpacePO> iterator = spaceList.iterator();
		while(iterator.hasNext()) {
			UserSpacePO space = iterator.next();
			/**
			if (space.getStartDate().compareTo(DateUtil.beginOfDay(new Date())) > 0) {
				//只有在车位生效之后才能转让
				throw new BusinessException("您的车位还没到生效日期,无法转让");
			} 
			**/
			
			if (space.getEndDate().compareTo(DateUtil.endOfDay(new Date())) <= 0) {
				//都是当天到期的，无法无法进行转赠
				iterator.remove();
			}
		}
		AssertUtil.checkTrue(!CollectionUtils.isEmpty(spaceList), "您的车位为当天到期或已到期，无法进行转赠");
		log.info("转让人车位信息：{}",JSON.toJSONString(spaceList));
		UserProfilePO user = userProfileService.selectUserProfileByNameAndJobNumber(null, inJobNum );
		AssertUtil.checkNull(user, "受让人不存在"); 
		List<UserVerifyPO> verifyList = userVerifyService.queryVerifyListByUserIdList(Arrays.asList(user.getUserId()));
		AssertUtil.checkTrue(!CollectionUtils.isEmpty(verifyList), "受让人未绑定车辆,无法转赠");
		
		LotteryBlackListPO black =  lotteryBlackListService.queryBlackUserInfo(user.getUserId());
		AssertUtil.checkTrue(!(black != null), "受让人为黑名单用户,无法转赠");
		//执行转让
		lotteryDealService.transfer(spaceList,verifyList,inJobNum);
	}

    /**
     * 查询车位转赠记录
     * 
     * @param id 车位转赠记录主键
     * @return 车位转赠记录
     */
//    @Override
//    public ParkingSpaceTransferRecord selectParkingSpaceTransferRecordById(Long id)
//    {
//        return parkingSpaceTransferRecordMapper.selectParkingSpaceTransferRecordById(id);
//    }
    

    /**
     * 查询车位转赠记录列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<ParkingSpaceTransferRecordBO> getParkingSpaceTransferRecordList(ParkingSpaceTransferRecordDTO dto) {
        Page<ParkingSpaceTransferRecordPO> page = PageUtils.toPage(dto);
		ParkingSpaceTransferRecordPO queryPO = new ParkingSpaceTransferRecordPO();
		BeanUtils.copyProperties(dto,queryPO);

		IPage selectPage = parkingSpaceTransferRecordMapper.selectParkingSpaceTransferRecordPage(page,queryPO,dto.getUserInfo());
		if (selectPage.getTotal() == 0){
			return PageUtils.emptyResponseList(selectPage);
		}
		List<ParkingSpaceTransferRecordBO> boList = BeanConvertorUtils.copyList(selectPage.getRecords(), ParkingSpaceTransferRecordBO.class);
		return PageUtils.toResponseList(page,boList);
    }

	private void setParkingLotRegionByCode(ParkingSpaceTransferRecordBO bo) {
		ParkingLotPO parkingLotPO = parkingLotService.selectParkingLotByCode(bo.getParkingLotCode());
		if (parkingLotPO == null) {
			return;
		}
		bo.setParkingLotRegion(parkingLotPO.getRegion());
	}

}
