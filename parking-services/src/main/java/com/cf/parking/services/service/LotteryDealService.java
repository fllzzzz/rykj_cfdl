package com.cf.parking.services.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.lang3.time.DateUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import com.alibaba.fastjson.JSON;
import com.cf.parking.dao.po.LotteryApplyRecordPO;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.dao.po.ParkingSpaceTransferRecordPO;
import com.cf.parking.dao.po.UserProfilePO;
import com.cf.parking.dao.po.UserSpacePO;
import com.cf.parking.dao.po.UserVerifyPO;
import com.cf.parking.services.constant.ParkingConstants;
import com.cf.parking.services.enums.UserSpaceStateEnum;
import com.cf.parking.services.enums.UserSpaceTypeEnum;
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
	private ParkingSpaceTransferRecordService parkingSpaceTransferRecordService;
	
	@Resource
	private UserSpaceService userSpaceService;
	
	@Resource
	private LotteryApplyRecordService lotteryApplyRecordService;
	
	@Resource
	private UserVerifyService userVerifyService;
	
	@Resource
	private UserProfileService userProfileService;
	
	
	
	
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
	 * @param outSpaceList 转让人车位
	 * @param verifyList 受让人车牌
	 * @param inJobNum 受让人工号
	 * 
	 *  1.受让人无车位且无公共车位，插入带定时日期的数据；
  		2.受让人仅有公共车位--->更改公共车位截止日期为今天，其余信息不改;插入带定时日期的数据，利用闸机车牌覆盖策略，覆盖掉公共车位
  		3.受让人有车位，3.1判断两者日期不可重叠，3.2判断是否同一车库，是的话根据日期判断更改开始或结束日期，不为同一车库则插入带定时日期的数据
	 */
	@Transactional(rollbackFor = Exception.class)
	public void transfer(List<UserSpacePO> outSpaceList, List<UserVerifyPO> verifyList, String inJobNum) {
		log.info("转让人车位：{},受让人车牌：{},受让人工号：{}",JSON.toJSONString(outSpaceList),JSON.toJSONString(verifyList),inJobNum);
		List<UserSpacePO> addList = new ArrayList<>();
		List<UserSpacePO> updateList = new ArrayList<>();
		
		//获取转让人
		UserProfilePO outUser = userProfileService.selectUserProfileByNameAndJobNumber(null, outSpaceList.get(0).getJobNumber() );
		//转让记录
		List<ParkingSpaceTransferRecordPO> transferList = new ArrayList<>();
		
		//获取车牌集合
		List<String> plateList = verifyList.stream().map(item -> item.getPlateNo()).collect(Collectors.toList());
		
		String userName = verifyList.get(0).getUserName();
		//用户已有车库、有效期
		List<UserSpacePO> spaceList = userSpaceService.querySpaceGroupByExpireDate(inJobNum,UserSpaceTypeEnum.LOTTERY.getState());
		log.info("受让人持有的车位：{}",spaceList);
		//受让人拥有的车库
		List<String> parkLotList = spaceList.stream().map(item -> item.getParkingLot()).collect(Collectors.toList());
		
		if (CollectionUtils.isEmpty(spaceList)) { //受让人无摇号车位

			Date minDate = DateUtil.beginOfDay(new Date());
			Date maxDate = DateUtil.beginOfDay(new Date());
			
			for(UserSpacePO outSpace : outSpaceList ) {
				//取最小的日期
				minDate = minDate.compareTo(outSpace.getStartDate()) > 0 ? outSpace.getStartDate() : minDate;
				maxDate = maxDate.compareTo(outSpace.getEndDate()) > 0 ?  maxDate : outSpace.getEndDate();
				transferList.add(initTransferInfo(outSpace,outUser.getUserId(), verifyList.get(0).getUserId(),verifyList.get(0).getUserName()));
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
								.setType(UserSpaceTypeEnum.LOTTERY.getState())
								.setBatchId(outSpace.getBatchId())
								.setBatchNum(outSpace.getBatchNum())
								.setRoundId(outSpace.getRoundId());
						addList.add(po);
					});
			}
			
			List<UserSpacePO> alloSpaceList = userSpaceService.querySpaceGroupByExpireDate(inJobNum,UserSpaceTypeEnum.SETTING.getState());
			if (!CollectionUtils.isEmpty(alloSpaceList)) {
				for (UserSpacePO space : alloSpaceList) {
					if (maxDate.compareTo(space.getEndDate()) < 0) {//可能是月末转让，且第二个月受让者又没中，导致转让的有效期小于公共车位有效期
						//更新起始日期为下有效期截止日期+1
						Date startDate = DateUtils.addDays(maxDate, 1);
						userSpaceService.updateStartDate(inJobNum,startDate, DateUtil.format(startDate, ParkingConstants.SHORT_DATE_FORMAT), UserSpaceTypeEnum.SETTING.getState());
					}
				}
				
			}
			
			
		} else {
		  /**
			* 我们的日期都是整月出现的，且下一期的有效期开始时间大于上一期的结束时间，在批次设置里有校验
			* 所以日期上会出现4种情况
			* 1.转让车位的有效期是受让人持有车位有效期的子集，同一个车位不能再同一时间段又2个车位，所以不能转让
			* 2.受让人持有车位的有效期是转让车位有效期的子集，不让转让 (1和2总结就是日期不能有交集)
			* 3.转让车位的有效期的结束时间<持有车辆有效期的开始时间 ，同一个车库就合并车位有效期，否则就新增一条车位数据
			* 4.持有车位的有效期的结束时间< 转让车辆有效期的开始时间，同一个车库就合并车位有效期，否则就新增一条车位数据
			*/
			outSpaceList.forEach(outSpace -> { //校验有没有第一种和第二种情况
				spaceList.forEach(inSpace -> {
					
					if( !(inSpace.getStartDate().compareTo(outSpace.getEndDate()) > 0 ||
							outSpace.getStartDate().compareTo(inSpace.getEndDate()) > 0	)) {
						throw new BusinessException("双方停车场有效期不能存在重叠，无法转让");
					}
					
				});
			});
			
			//循环体中存车牌的临时变量
			final List<String> tempList = new ArrayList<>();
			//受让人车库下的车位信息
			List<UserSpacePO> parkSpaceList = new ArrayList<>();
			//根据转让的车库和有效期进行初始化车位数据
			for(UserSpacePO outSpace : outSpaceList) {
				transferList.add(initTransferInfo(outSpace,outUser.getUserId(), verifyList.get(0).getUserId(),verifyList.get(0).getUserName()));
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
									.setType(UserSpaceTypeEnum.LOTTERY.getState())
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
									.setType(UserSpaceTypeEnum.LOTTERY.getState())
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
								.setType(UserSpaceTypeEnum.LOTTERY.getState())
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
								.setType(UserSpaceTypeEnum.LOTTERY.getState())
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
		
		if (!CollectionUtils.isEmpty(transferList)) {
			parkingSpaceTransferRecordService.saveBatch(transferList);
		}
		
		//把转让人的车位有效期更改为当前日期和车位生效日的较大者(就是当月转让下月生效的数据)
		userSpaceService.updateEndDate(outSpaceList.get(0).getJobNumber(),new Date().compareTo(outSpaceList.get(0).getStartDate()) >= 0 ? new Date() : outSpaceList.get(0).getStartDate() );
		
	}

	/**
	 * 生成转让记录
	 * @param userId 转让人
	 * @param outSpace 转让车位
	 * @param acceptId 受让人
	 * @param acceptName
	 * @return
	 */
	private ParkingSpaceTransferRecordPO initTransferInfo(UserSpacePO outSpace,Long userId, Long acceptId, String acceptName) {
		return new ParkingSpaceTransferRecordPO()
				.setAcceptUserId(acceptId)
				.setAcceptUserName(acceptName)
				.setCreateTm(new Date())
				.setId(idWorker.nextId())
				.setParkingLotCode(outSpace.getParkingLot())
				.setUpdateTm(new Date())
				.setUserId(userId)
				.setValidEndDate(outSpace.getEndDate())
				.setValidStartDate(outSpace.getStartDate())				;
	}

	/**
	 * 根据批次给未中签的人员分配停车场
	 * @param batch 批次数据
	 * @param parking 停车场数据
	 */
	public void allocationPark(LotteryBatchPO batch, ParkingLotPO parking) {
		//获取到报名人员
		List<LotteryApplyRecordPO> applyList = lotteryApplyRecordService.queryLotteryApplyList(batch.getId() ,null);
		//获取中签人员
		List<String> jobNumList = lotteryResultDetailService.querySpaceListByBatchId(batch.getId());
		//过滤掉中签人员
		applyList = applyList.stream().filter(apply -> !jobNumList.contains(apply.getJobNumber())).collect(Collectors.toList());
		if(CollectionUtils.isEmpty(applyList)) {
			log.info("批次：{}不存在未中签人员",batch.getId());
			return;
		}
		jobNumList.clear();;
		List<String> applyJobList = applyList.stream().map(apply -> apply.getJobNumber()).collect(Collectors.toList());
		log.info("未中签人员工号：{}",applyJobList);	
		//查询未中签用户是否有车位
		List<UserSpacePO> existSpaceList = userSpaceService.querySpaceListByJobNum(applyJobList, UserSpaceTypeEnum.LOTTERY.getState());
		List<UserSpacePO> alloSpaceList = userSpaceService.querySpaceListByJobNum(applyJobList, UserSpaceTypeEnum.SETTING.getState());
		//查询用户
		List<UserProfilePO> userList = userProfileService.getProfileListByJobNumList(applyJobList);
		List<Long> userIdList = userList.stream().map(user -> user.getUserId()).collect(Collectors.toList());
		//车牌列表
		List<UserVerifyPO> vefifyList = userVerifyService.queryVerifyListByUserIdList(userIdList);
		userIdList.clear();
		userList.clear();
		applyJobList.clear();
		
		List<UserSpacePO> existList = new ArrayList<>();
		List<UserSpacePO> addList = new ArrayList<>();
		
		generateSpaceList(batch,parking,applyList,existSpaceList,alloSpaceList,vefifyList,existList,addList);
		
		if (!CollectionUtils.isEmpty(addList)) {
			userSpaceService.saveBatch(addList);
		}
		
		if (!CollectionUtils.isEmpty(existList)) {
			userSpaceService.updateBatchById(existList);
		}
		
	}

	/**
	 * 生成车位信息数据
	 * @param batch			  批次数据
	 * @param parking 
	 * @param applyList 	  报名数据
	 * @param existSpaceList  摇号车位
	 * @param alloSpaceList   分配车位
	 * @param vefifyList 	  车牌信息
	 * @param addList 	 待保存的车位
	 * @param existList  待更新的车位
	 */
	private void generateSpaceList(LotteryBatchPO batch, ParkingLotPO parking, List<LotteryApplyRecordPO> applyList,
			List<UserSpacePO> existSpaceList, List<UserSpacePO> alloSpaceList, List<UserVerifyPO> vefifyList,
			List<UserSpacePO> existList, List<UserSpacePO> addList) {
		log.info("分配车位入参：停车场：{}，批次：{}，未中签人员：{}，已存在车位：{}，已存在分配车位：{}，车牌信息：{}",JSON.toJSONString(parking),
				JSON.toJSONString(batch),JSON.toJSONString(applyList),JSON.toJSONString(existSpaceList),JSON.toJSONString(alloSpaceList),
				JSON.toJSONString(vefifyList)
				);
		//车位信息映射成Map形式  {jobNum:{plateNo:UserSpacePO}}
		Map<String/**工号*/, Map<String/**车牌*/, UserSpacePO>> userSpaceMap = (Map<String, Map<String, UserSpacePO>>) existSpaceList.stream()
			.collect(Collectors.toMap(UserSpacePO::getJobNumber, // 工号作为外层Map的键
				   userSpacePO -> {
				            Map<String, UserSpacePO> innerMap = new HashMap<>();
				            innerMap.put(userSpacePO.getPlateNo(), userSpacePO); // 使用车牌作为内层Map的键
				            return innerMap;
				       },
				       (existingMap, newMap) -> {
				            existingMap.putAll(newMap); // 合并现有Map和新Map中的元素
				            return existingMap;
				       }));
		existSpaceList.clear();
		
		Map<String/**工号*/, Map<String/**车牌*/, UserSpacePO>> alloSpaceMap = (Map<String, Map<String, UserSpacePO>>) alloSpaceList.stream()
				.collect(Collectors.toMap(UserSpacePO::getJobNumber, // 工号作为外层Map的键
					   userSpacePO -> {
					            Map<String, UserSpacePO> innerMap = new HashMap<>();
					            innerMap.put(userSpacePO.getPlateNo(), userSpacePO); // 使用车牌作为内层Map的键
					            return innerMap;
					       },
					       (existingMap, newMap) -> {
					            existingMap.putAll(newMap); // 合并现有Map和新Map中的元素
					            return existingMap;
					       }));
		alloSpaceList.clear();
		//车牌信息映射成用户id--车牌集合形式
		Map<Long,List<String>> verifyMap = vefifyList.stream().collect(Collectors.groupingBy(UserVerifyPO::getUserId ,Collectors.mapping(UserVerifyPO::getPlateNo, Collectors.toList())));
		vefifyList.clear();
		
		//临时存放车牌数据
		List<String> plateNoList = new ArrayList<>();
		//存放车牌---车位对应信息
		Map<String, UserSpacePO> plateMap = new HashMap<>();
		//存放有效期和批次有效期一致的车位数据
		List<UserSpacePO> canSkipList = new ArrayList<>();
		//存放临时车位信息
		UserSpacePO tempSpace = null;
		for(LotteryApplyRecordPO apply : applyList){
			
			plateNoList = verifyMap.get(apply.getUserId());
			if (CollectionUtils.isEmpty(plateNoList)) {
				log.error("工号为：{}的用户无车牌",apply.getJobNumber());
				continue;
			}
			
			if (userSpaceMap.containsKey(apply.getJobNumber())) {
				//存在车位
				//两种情况，1.上一次的车位还未过期，这时的车位应该加上定时日期。2.有人给他转车位了，这时应该跳过。
				plateMap = userSpaceMap.get(apply.getJobNumber());
				//筛选出车位有效期和当前批次一致的数据
				canSkipList = plateMap.values().stream().filter(space -> DateUtil.beginOfDay(space.getEndDate()).compareTo(DateUtil.beginOfDay(batch.getValidEndDate())) == 0 ).collect(Collectors.toList());
				if(!CollectionUtils.isEmpty(canSkipList)) { //不为空，则是第二种（有人给他转车位了）
					log.info("用户：{}已有车位：{},无需分配公共车位",apply.getJobNumber(),JSON.toJSONString(canSkipList));
					continue;
				} else {
					addSpaceList(addList,batch,apply,plateNoList,DateUtil.format(batch.getValidStartDate(), ParkingConstants.SHORT_DATE_FORMAT),parking.getRegionCode());
				}
			} else if (alloSpaceMap.containsKey(apply.getJobNumber())){//存在公共车位 
				plateMap = alloSpaceMap.get(apply.getJobNumber());
				for(String plateNo : plateNoList) {
					tempSpace = plateMap.get(plateNo);//获取车牌对应的车库
					updateSpaceList(addList,existList,batch,apply,tempSpace,plateNo,parking.getRegionCode());
				}
			} else {
				addSpaceList(addList,batch,apply,plateNoList,null,parking.getRegionCode());
			}
			
		}
	}

	/**
	 * tempSpace存在时，判断车库和入参parkCode是否一致，一致时放到修改列表，不一致放到新增列表
	 * tempSpace不存在，放到新增列表
	 * @param addList 待保存的车位集合
	 * @param existList 待修改的车位集合
	 * @param batch 分配的批次
	 * @param apply 报名信息
	 * @param tempSpace 已有的分配车位
	 * @param plateNo 车牌好
	 * @param parkCode 车库编码
	 */
	private void updateSpaceList(List<UserSpacePO> addList, List<UserSpacePO> existList, LotteryBatchPO batch,
			LotteryApplyRecordPO apply, UserSpacePO tempSpace, String plateNo, String parkCode) {
		if(tempSpace != null && tempSpace.getParkingLot().equals(parkCode)) { //存在车位且车库一致，更新
			tempSpace.setEndDate(batch.getValidEndDate())
					.setScheduleDate("")
					.setState(UserSpaceStateEnum.UNSYNC.getState())
					.setUpdateTm(new Date())
					.setBatchId(batch.getId())
					.setType(UserSpaceTypeEnum.SETTING.getState());
			existList.add(tempSpace);
			return ;
		}
		
		UserSpacePO space = new UserSpacePO()
				.setBatchId(batch.getId())
				.setType(UserSpaceTypeEnum.SETTING.getState())
				.setBatchNum(batch.getBatchNum())
				.setCreateTm(new Date())
				.setEndDate(batch.getValidEndDate())
				.setJobNumber(apply.getJobNumber())
				.setName(apply.getUserName())
				.setParkingLot(parkCode)
				.setPlateNo(plateNo)
				//走到这里，代表这个人是有车位的，只是没有这个车牌,或者是这个车牌有车库只是车库不一致。没有这个车牌就不需要定时新增，车库不一致需要定时新增
				.setScheduleDate(tempSpace == null ? null : DateUtil.format(batch.getValidStartDate(), ParkingConstants.SHORT_DATE_FORMAT))
				.setStartDate(batch.getValidStartDate())
				.setState(UserSpaceStateEnum.UNSYNC.getState())
				.setUpdateTm(new Date())
				.setUserSpaceId(idWorker.nextId())
				;
		addList.add(space);
		
	}

	/**
	 * 往待保存集合addList中添加数据
	 * @param addList
	 * @param batch
	 * @param apply 
	 * @param plateNoList
	 */
	private void addSpaceList(List<UserSpacePO> addList, LotteryBatchPO batch, LotteryApplyRecordPO apply, List<String> plateNoList,String scheduleDate,String parkCode) {
		plateNoList.forEach(plate -> {
			UserSpacePO space = new UserSpacePO()
					.setBatchId(batch.getId())
					.setType(UserSpaceTypeEnum.SETTING.getState())
					.setBatchNum(batch.getBatchNum())
					.setCreateTm(new Date())
					.setEndDate(batch.getValidEndDate())
					.setJobNumber(apply.getJobNumber())
					.setName(apply.getUserName())
					.setParkingLot(parkCode)
					.setPlateNo(plate)
					.setScheduleDate(scheduleDate)
					.setStartDate(batch.getValidStartDate())
					.setState(UserSpaceStateEnum.UNSYNC.getState())
					.setUpdateTm(new Date())
					.setUserSpaceId(idWorker.nextId())
					;
			addList.add(space);
		});
		
		
	}
			
	

}
