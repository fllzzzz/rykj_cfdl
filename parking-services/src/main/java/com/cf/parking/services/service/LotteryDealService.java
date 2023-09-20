package com.cf.parking.services.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import com.alibaba.fastjson.JSON;
import com.cf.parking.dao.po.LotteryApplyRecordPO;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.dao.po.UserSpacePO;
import com.cf.parking.dao.po.UserVerifyPO;
import com.cf.parking.services.constant.ParkingConstants;
import com.cf.parking.services.enums.UserSpaceStateEnum;
import com.cf.support.bean.IdWorker;
import com.cf.support.exception.BusinessException;
import cn.hutool.core.date.DateUtil;
import lombok.extern.slf4j.Slf4j;




@Service
@Slf4j
public class LotteryDealService {

	@Resource
	private IdWorker idWorker;
	
	@Resource
	private LotteryResultDetailService lotteryResultDetailService; 
	
	@Resource
	private LotteryResultService lotteryResultService; 
	
	
	@Resource
	private UserSpaceService userSpaceService;
	
	/**
	 * 摇号
	 * @param batch
	 * @param lottery
	 * @param round
	 */

	public void doLottery(LotteryBatchPO batch, LotteryResultPO lottery, ParkingLotPO parkLot,
			List<LotteryApplyRecordPO> parkApplyList) {
		log.info("摇号入参对象：batch:{},lottery:{},parking:{},employee:{}",JSON.toJSONString(batch),JSON.toJSONString(lottery),JSON.toJSONString(parkLot),JSON.toJSONString(parkApplyList));
		if (CollectionUtils.isEmpty(parkApplyList)) {
			throw new BusinessException("没有符合摇号条件的人员");
		}
		
		//存放中签结果
		List<LotteryResultDetailPO> result = new ArrayList<>();
		
		//车位比摇号人数量多，全部中签
		if (parkLot.getAmount() >= parkApplyList.size()) {
			parkApplyList.forEach(apply -> {
				LotteryResultDetailPO detail = generateLotteryResult(apply,parkLot.getRegionCode(),lottery.getId());
				result.add(detail);
			});
		} else {
			//存放中签序号
			Set<Integer> indexSet = generateLotteryIndex(parkLot.getAmount(),parkApplyList.size());
			indexSet.forEach(index -> {
				LotteryResultDetailPO detail = generateLotteryResult(parkApplyList.get(index),parkLot.getRegionCode(),lottery.getId());
				result.add(detail);
			});
		}
		log.info("摇号结果入库：{}",JSON.toJSONString(result));
		lotteryResultDetailService.saveBatch(result);
	}

	/**
	 * 封装结果明细数据
	 * @param apply
	 * @param parkLotCode
	 * @param resultId
	 * @return
	 */
	private LotteryResultDetailPO generateLotteryResult(LotteryApplyRecordPO apply,String parkLotCode,Long resultId) {
		LotteryResultDetailPO detail = new LotteryResultDetailPO();
		detail.setCreateTm(new Date()).setId(idWorker.nextId()).setParkingLotCode(parkLotCode)
		.setResultId(resultId).setUpdateTm(new Date()).setUserId(apply.getUserId())
		.setUserName(apply.getUserName()).setUserJobNumber(apply.getJobNumber());
		return detail;
	}
	
	/**
	 * 随机抽取报名摇号序号
	 * @param amount 中签数量
	 * @param total  参与数量
	 * @return
	 */
	private static Set<Integer> generateLotteryIndex(long amount, int total) {
		Random random = new Random();
		Set<Integer> result = new HashSet<>();
		while(result.size() != amount) {
			result.add(random.nextInt(total));
		}
		return result;
	}

	/**
	 * 执行转让操作
	 * @param spaceList 转让人车位
	 * @param verifyList 受让人车牌
	 * @param inJobNum 受让人工号
	 */
	public void transfer(List<UserSpacePO> outSpaceList, List<UserVerifyPO> verifyList, String inJobNum) {
		log.info("转让人车位：{},受让人车位：{},受让人工号：{}",JSON.toJSONString(outSpaceList),JSON.toJSONString(verifyList),inJobNum);
		List<UserSpacePO> addList = new ArrayList<>();
		List<UserSpacePO> updateList = new ArrayList<>();
		
		//获取车牌集合
		List<String> plateList = verifyList.stream().map(item -> item.getPlateNo()).collect(Collectors.toList());
		
		String userName = verifyList.get(0).getUserName();
		//用户已有车库、有效期
		List<UserSpacePO> spaceList = userSpaceService.querySpaceGroupByExpireDate(inJobNum);
		log.info("受让人持有的车位：{}",spaceList);
		//受让人拥有的车库
		List<String> parkLotList = spaceList.stream().map(item -> item.getParkingLot()).collect(Collectors.toList());
		
		if (CollectionUtils.isEmpty(spaceList)) { //受让人无车位
			for(UserSpacePO outSpace : outSpaceList ) {
				verifyList.forEach(verify -> {
						UserSpacePO po = new UserSpacePO()
								.setCreateTm(new Date())
								.setUpdateTm(new Date())
								.setUserSpaceId(idWorker.nextId())
								.setJobNumber(inJobNum)
								.setName(userName)
								.setPlateNo(verify.getPlateNo())
								.setParkingLot(outSpace.getParkingLot())
								//转让人车位起始日期比今天晚的话就取转让人车位起始日期，否则就取当前日期+1的日期
								.setScheduleDate(outSpace.getStartDate().compareTo(DateUtil.endOfDay(new Date())) > 0 ? DateUtil.format(outSpace.getStartDate(), ParkingConstants.SHORT_DATE_FORMAT)  :  DateUtil.format(DateUtil.beginOfDay( DateUtil.tomorrow()), ParkingConstants.SHORT_DATE_FORMAT))
								.setStartDate(outSpace.getStartDate())
								.setEndDate(outSpace.getEndDate())
								.setState(UserSpaceStateEnum.UNSYNC.getState())
								.setBatchId(outSpace.getBatchId())
								.setBatchNum(outSpace.getBatchNum())
								.setRoundId(outSpace.getRoundId());
						addList.add(po);
					});
			}
					
		} else {
		  /**
			* 我们的日期都是整月出现的，且下一期的有效期开始时间大于上一期的结束时间，在批次设置里有校验
			* 所以日期上会出现4种情况
			* 1.转让车位的有效期是受让人持有车位有效期的子集，同一个车位不能再同一时间段又2个车位，所以不能转让
			* 2.受让人持有车位的有效期是转让车位有效期的子集，不让转让
			* 3.转让车位的有效期的结束时间<持有车辆有效期的开始时间 ，同一个车库就合并车位有效期，否则就新增一条车位数据
			* 4.持有车位的有效期的结束时间< 转让车辆有效期的开始时间，同一个车库就合并车位有效期，否则就新增一条车位数据
			*/
			outSpaceList.forEach(outSpace -> { //校验有没有第一种和第二种情况
				spaceList.forEach(inSpace -> {
					if (inSpace.getEndDate().compareTo(outSpace.getEndDate()) >= 0 && 
							inSpace.getStartDate().compareTo(outSpace.getStartDate()) <= 0) {
						//转让人的车位有效期是受让人车位有效期的子集
						log.error("转让人车位={}的有效期属于受让人车位={}的一部分,无须进行转让");
						throw new BusinessException("受让人持有该有效期内的车位，无法进行转让");
					}
					
					if (inSpace.getEndDate().compareTo(outSpace.getEndDate()) <= 0 && 
							inSpace.getStartDate().compareTo(outSpace.getStartDate()) >= 0) {
						//转让人的车位有效期是受让人车位有效期的子集
						log.error("转让人车位={}的有效期属于受让人车位={}的一部分,无须进行转让");
						throw new BusinessException("受让人持有该有效期内的车位，无法进行转让");
					}
				});
			});
			
			//循环体中存车牌的临时变量
			final List<String> tempList = new ArrayList<>();
			//受让人车库下的车位信息
			List<UserSpacePO> parkSpaceList = new ArrayList<>();
			//根据转让的车库和有效期进行初始化车位数据
			for(UserSpacePO outSpace : outSpaceList) {
				if (parkLotList.contains(outSpace.getParkingLot())) {//有该车库车位
					tempList.addAll(plateList);
					//查出车位信息进行合并
					parkSpaceList = userSpaceService.querySpaceByJobNumAndParkLot(inJobNum,outSpace.getParkingLot());
					parkSpaceList.forEach(park -> {
						
						if (!plateList.contains(park.getPlateNo())) {//车牌号不在当前受让人车牌号列表中，忽略
							return ;
						}
						
						tempList.remove(park.getPlateNo());
						//3.转让车位的有效期的结束时间<持有车辆有效期的开始时间
						if (outSpace.getEndDate().compareTo(park.getStartDate()) < 0) {
							park.setUpdateTm(new Date())
									//转让人车位起始日期比今天晚的话就取转让人车位起始日期，否则就取当前日期+1的日期
									.setScheduleDate(outSpace.getStartDate().compareTo(DateUtil.endOfDay(new Date())) > 0 ? DateUtil.format(outSpace.getStartDate(), ParkingConstants.SHORT_DATE_FORMAT)  :  DateUtil.format(DateUtil.beginOfDay( DateUtil.tomorrow()), ParkingConstants.SHORT_DATE_FORMAT))
									.setStartDate(outSpace.getStartDate())
									.setState(UserSpaceStateEnum.UNSYNC.getState())
									.setBatchId(outSpace.getBatchId())
									.setBatchNum(outSpace.getBatchNum())
									.setRoundId(outSpace.getRoundId())
									;
							updateList.add(park);
						}
						//4.持有车位的有效期的结束时间< 转让车辆有效期的开始时间
						if (park.getEndDate().compareTo(outSpace.getStartDate()) < 0) {
							park.setUpdateTm(new Date())
									//转让人车位起始日期比今天晚的话就取转让人车位起始日期，否则就取当前日期+1的日期
									.setScheduleDate(outSpace.getStartDate().compareTo(DateUtil.endOfDay(new Date())) > 0 ? DateUtil.format(outSpace.getStartDate(), ParkingConstants.SHORT_DATE_FORMAT)  :  DateUtil.format(DateUtil.beginOfDay( DateUtil.tomorrow()), ParkingConstants.SHORT_DATE_FORMAT))
									.setEndDate(outSpace.getEndDate())
									.setState(UserSpaceStateEnum.UNSYNC.getState())
									.setBatchId(outSpace.getBatchId())
									.setBatchNum(outSpace.getBatchNum())
									.setRoundId(outSpace.getRoundId())
									;
							updateList.add(park);
						}
						
					});
					
					//tempList 不为空则代表尚有车牌未初始化
					tempList.forEach(plateNo -> {
						UserSpacePO po = new UserSpacePO()
								.setCreateTm(new Date())
								.setUpdateTm(new Date())
								.setUserSpaceId(idWorker.nextId())
								.setJobNumber(inJobNum)
								.setName(userName)
								.setPlateNo(plateNo)
								.setParkingLot(outSpace.getParkingLot())
								//转让人车位起始日期比今天晚的话就取转让人车位起始日期，否则就取当前日期+1的日期
								.setScheduleDate(outSpace.getStartDate().compareTo(DateUtil.endOfDay(new Date())) > 0 ? DateUtil.format(outSpace.getStartDate(), ParkingConstants.SHORT_DATE_FORMAT)  :  DateUtil.format(DateUtil.beginOfDay( DateUtil.tomorrow()), ParkingConstants.SHORT_DATE_FORMAT))
								.setStartDate(outSpace.getStartDate())
								.setEndDate(outSpace.getEndDate())
								.setState(UserSpaceStateEnum.UNSYNC.getState())
								.setBatchId(outSpace.getBatchId())
								.setBatchNum(outSpace.getBatchNum())
								.setRoundId(outSpace.getRoundId())
								;
						addList.add(po);
					});
					
					tempList.clear();
					
				} else {//无该车库
					verifyList.forEach(verify -> {
						UserSpacePO po = new UserSpacePO()
								.setCreateTm(new Date())
								.setUpdateTm(new Date())
								.setUserSpaceId(idWorker.nextId())
								.setJobNumber(inJobNum)
								.setName(userName)
								.setPlateNo(verify.getPlateNo())
								.setParkingLot(outSpace.getParkingLot())
								//转让人车位起始日期比今天晚的话就取转让人车位起始日期，否则就取当前日期+1的日期
								.setScheduleDate(outSpace.getStartDate().compareTo(DateUtil.endOfDay(new Date())) > 0 ? DateUtil.format(outSpace.getStartDate(), ParkingConstants.SHORT_DATE_FORMAT)  :  DateUtil.format(DateUtil.beginOfDay( DateUtil.tomorrow()), ParkingConstants.SHORT_DATE_FORMAT))
								.setStartDate(outSpace.getStartDate())
								.setEndDate(outSpace.getEndDate())
								.setState(UserSpaceStateEnum.UNSYNC.getState())
								.setBatchId(outSpace.getBatchId())
								.setBatchNum(outSpace.getBatchNum())
								.setRoundId(outSpace.getRoundId())
								;
						addList.add(po);
					});
				}
				
				
			}//循环转让车库结束
			
		}
		
		if (!CollectionUtils.isEmpty(addList)) {
			userSpaceService.saveBatch(addList);
		}
		
		if (!CollectionUtils.isEmpty(updateList)) {
			userSpaceService.updateBatchById(updateList);
		}
		
		//把转让人的车位有效期更改为今天
		userSpaceService.updateEndDate(outSpaceList.get(0).getJobNumber(),new Date());
	}
			
	

}
