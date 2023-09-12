package com.cf.parking.services.facade.impl;

import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.annotation.Resource;

import com.cf.parking.dao.mapper.ParkingSpaceTransferRecordMapper;
import com.cf.parking.dao.po.LotteryBlackListPO;
import com.cf.parking.dao.po.UserPO;
import com.cf.parking.dao.po.UserSpacePO;
import com.cf.parking.dao.po.UserVerifyPO;
import com.cf.parking.facade.facade.ParkingSpaceTransferRecordFacade;
import com.cf.parking.services.service.LotteryBlackListService;
import com.cf.parking.services.service.LotteryDealService;
import com.cf.parking.services.service.UserService;
import com.cf.parking.services.service.UserSpaceService;
import com.cf.parking.services.service.UserVerifyService;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.exception.BusinessException;

import cn.hutool.core.date.DateUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

/**
 * 车位转赠记录Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
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
    private UserService userService;
    
    @Resource
    private LotteryBlackListService lotteryBlackListService;
    
    @Resource
    private LotteryDealService lotteryDealService;
    
	@Override
	public void transfer(String outJobNum, String inJobNum) {
		List<UserSpacePO> spaceList = userSpaceService.querySpaceGroupByExpireDate(outJobNum);

		AssertUtil.checkTrue(!CollectionUtils.isEmpty(spaceList), "该用户无车位，无法进行转赠");
		//移除当天到期的车位信息
		Iterator<UserSpacePO> iterator = spaceList.iterator();
		while(iterator.hasNext()) {
			UserSpacePO space = iterator.next();
			if (space.getEndDate().compareTo(DateUtil.endOfDay(new Date())) <= 0) {
				//都是当天到期的，无法无法进行转赠
				iterator.remove();
			}
		}
		AssertUtil.checkTrue(!CollectionUtils.isEmpty(spaceList), "该用户车位为当天到期或已到期，无法进行转赠");
		
		UserPO user = userService.selectByOpenId(inJobNum);
		AssertUtil.checkNull(user, "受让人不存在");
		List<UserVerifyPO> verifyList = userVerifyService.queryVerifyListByUserIdList(List.of(user.getUserId()));
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
     * 
     * @param parkingSpaceTransferRecord 车位转赠记录
     * @return 车位转赠记录
     */
//    @Override
//    public List<ParkingSpaceTransferRecord> selectParkingSpaceTransferRecordList(ParkingSpaceTransferRecord parkingSpaceTransferRecord)
//    {
//        return parkingSpaceTransferRecordMapper.selectParkingSpaceTransferRecordList(parkingSpaceTransferRecord);
//    }

    /**
     * 新增车位转赠记录
     * 
     * @param parkingSpaceTransferRecord 车位转赠记录
     * @return 结果
     */
//    @Override
//    public int insertParkingSpaceTransferRecord(ParkingSpaceTransferRecord parkingSpaceTransferRecord)
//    {
//        return parkingSpaceTransferRecordMapper.insertParkingSpaceTransferRecord(parkingSpaceTransferRecord);
//    }

    /**
     * 修改车位转赠记录
     * 
     * @param parkingSpaceTransferRecord 车位转赠记录
     * @return 结果
     */
//    @Override
//    public int updateParkingSpaceTransferRecord(ParkingSpaceTransferRecord parkingSpaceTransferRecord)
//    {
//        return parkingSpaceTransferRecordMapper.updateParkingSpaceTransferRecord(parkingSpaceTransferRecord);
//    }

    /**
     * 批量删除车位转赠记录
     * 
     * @param ids 需要删除的车位转赠记录主键
     * @return 结果
     */
//    @Override
//    public int deleteParkingSpaceTransferRecordByIds(Long[] ids)
//    {
//        return parkingSpaceTransferRecordMapper.deleteParkingSpaceTransferRecordByIds(ids);
//    }

    /**
     * 删除车位转赠记录信息
     * 
     * @param id 车位转赠记录主键
     * @return 结果
     */
//    @Override
//    public int deleteParkingSpaceTransferRecordById(Long id)
//    {
//        return parkingSpaceTransferRecordMapper.deleteParkingSpaceTransferRecordById(id);
//    }
}
