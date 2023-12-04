package com.cf.parking.services.service;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.bo.ParkBaseDetailRespBO;
import com.cf.parking.facade.bo.ParkBaseRespBO;
import com.cf.parking.facade.bo.ParkingCarInfoBO;
import com.cf.parking.facade.bo.ParkingCarQueryRespBO;
import com.cf.parking.facade.bo.ParkingYardBO;
import com.cf.parking.services.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.UserSpaceMapper;
import com.cf.parking.dao.po.EmployeePO;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.dao.po.UserSpacePO;
import com.cf.parking.dao.po.UserVerifyPO;
import com.cf.parking.facade.dto.ParkingCarQueryDTO;
import com.cf.parking.facade.dto.ParkingDeleteCarDTO;
import com.cf.parking.facade.dto.UserSpaceDTO;
import com.cf.parking.facade.dto.UserSpaceFuncTimeDTO;
import com.cf.parking.facade.dto.UserSpacePageDTO;
import com.cf.parking.facade.dto.UserSpaceValidityDTO;
import com.cf.parking.services.constant.ParkingConstants;
import com.cf.parking.services.enums.ParkingRemoteCodeEnum;
import com.cf.parking.services.enums.UserSpaceStateEnum;
import com.cf.parking.services.enums.UserSpaceTypeEnum;
import com.cf.parking.services.integration.ParkInvokeService;
import com.cf.support.bean.IdWorker;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.DingAlarmUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import javax.annotation.Resource;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author lpy
 * @date 2023-03-27 16:56:05
 * @description 用户车位表
 */
@Service
@Slf4j
public class UserSpaceService extends ServiceImpl<UserSpaceMapper, UserSpacePO> implements IService<UserSpacePO> {
    // 每次批量保存最大的数量
    private final Integer MAX_BATCH_SAVE_NUM = 500;

    @Resource
	private IdWorker idWorker;

    @Resource
    private UserSpaceService userSpaceService;
    @Resource
    private UserSpaceMapper userSpaceMapper;

    @Resource
    private ParkInvokeService parkInvokeService;
    
    @Resource
    private ParkingLotService parkingLotService;
    
    @Resource
	private ParkingInitService parkingInitService;
    
    @Resource
    private EmployeeService employeeService;
    
    
    /**
     * 获取请求数据，返回数据总大小
     *
     * @param data data
     * @return total
     */
    public List<UserSpacePO> getUserSpaceList(PageResponse<UserSpaceDTO> data) {

        List<UserSpaceDTO> list = data.getList();

        LinkedList<UserSpacePO> spacePOS = new LinkedList<>();
        for (UserSpaceDTO userSpaceDTO : list) {
            UserSpacePO userSpacePO = new UserSpacePO()
                    .setPlateNo(userSpaceDTO.getPlateNo())
                    .setJobNumber(userSpaceDTO.getPersonId())
                    .setName(userSpaceDTO.getPersonName());
            List<UserSpaceValidityDTO> validityList = userSpaceDTO.getValidity();

            // validityList可能为空
            if (CollectionUtils.isEmpty(validityList)) {
                continue;
            }

            StringBuffer sb = new StringBuffer();
            String startTime = "";
            String endTime = "";

            // 一般大小就一个
            for (UserSpaceValidityDTO userSpaceValidityDTO : validityList) {
                sb.append(userSpaceValidityDTO.getParkName()).append(",");

                List<UserSpaceFuncTimeDTO> functionTime = userSpaceValidityDTO.getFunctionTime();
                UserSpaceFuncTimeDTO eachFuncTime = functionTime.get(0);
                // funcTime数组就一个大小
                startTime = eachFuncTime.getStartTime();
                endTime = eachFuncTime.getEndTime();
            }
            userSpacePO.setStartDate(DateUtil.parse(startTime, ParkingConstants.SHORT_DATE_FORMAT));
            DateTime endDate = DateUtil.parse(endTime, ParkingConstants.SHORT_DATE_FORMAT);
            userSpacePO.setEndDate(endDate);
            // 精确到天，结束时间大于等于当前时间
            if (!this.judgeDateBefore(new Date(), endDate)) {
                continue;
            }
            // 如果多个车场拼接,去掉最后一个逗号
            String parkingLots = sb.toString();
            if (parkingLots.endsWith(",")) {
                parkingLots = parkingLots.substring(0, parkingLots.length() - 1);
            }
            userSpacePO.setParkingLot(parkingLots);
            spacePOS.add(userSpacePO);
        }
        return spacePOS;
    }


    /**
     * 分页查询 更新时间倒序
     * 所属车场（模糊）、工号、姓名、车牌号、有效起始日期、有效截止日期、
     *
     * @param param
     * @return
     */
    public IPage getUserSpacePage(UserSpacePageDTO param) {
        LambdaQueryWrapper<UserSpacePO> queryWrapper = new LambdaQueryWrapper<UserSpacePO>()
                .like(!StringUtils.isEmpty(param.getParkingLot()), UserSpacePO::getParkingLot, param.getParkingLot())
                .eq(!StringUtils.isEmpty(param.getJobNumber()), UserSpacePO::getJobNumber, param.getJobNumber())
                .eq(!StringUtils.isEmpty(param.getPlateNo()), UserSpacePO::getPlateNo, param.getPlateNo())
                .le(!ObjectUtils.isEmpty(param.getEndDate()), UserSpacePO::getEndDate, param.getEndDate())
                .orderByDesc(UserSpacePO::getUpdateTm);
        Page page = new Page().setCurrent(param.getPageNo()).setSize(param.getPageSize());
        return userSpaceMapper.selectPage(page, queryWrapper);
    }

    /**
     * 列表查询 更新时间倒序
     * 所属车场（模糊）、工号、姓名、车牌号、有效起始日期、有效截止日期、
     *
     * @param param
     * @return
     */
    public List<UserSpacePO> getUserSpaceList(UserSpacePageDTO param) {

        return userSpaceMapper.selectList(new LambdaQueryWrapper<UserSpacePO>()
                .like(!StringUtils.isEmpty(param.getParkingLot()), UserSpacePO::getParkingLot, param.getParkingLot())
                .eq(!StringUtils.isEmpty(param.getJobNumber()), UserSpacePO::getJobNumber, param.getJobNumber())
                .eq(!StringUtils.isEmpty(param.getPlateNo()), UserSpacePO::getPlateNo, param.getPlateNo())
                .ge(!ObjectUtils.isEmpty(param.getStartDate()), UserSpacePO::getStartDate, param.getStartDate())
                .le(!ObjectUtils.isEmpty(param.getEndDate()), UserSpacePO::getEndDate, param.getEndDate())
                .orderByDesc(UserSpacePO::getUpdateTm));
    }

    /**
     * 分批保存 500一批
     *
     * @param list
     */
    private void saveData(List<UserSpacePO> list) {
        // 分批保存
        for (int i = 0; i < list.size(); i += MAX_BATCH_SAVE_NUM) {
            List<UserSpacePO> batchList = list.subList(i, Math.min(i + MAX_BATCH_SAVE_NUM, list.size()));
            userSpaceService.saveBatch(batchList);
        }
    }


    /**
     * 将车牌号多个车场去重合并  (1:四楼，五楼，产品 2:五楼，产品 3：老院区）
     *
     * @param list
     */
    public void mergeParkingLots(List<UserSpacePO> list) {
        Map<String, List<UserSpacePO>> collect = list.stream().distinct().collect(Collectors.groupingBy(UserSpacePO::getPlateNo));
        ArrayList<UserSpacePO> resultList = new ArrayList<>(2000);
        collect.forEach((key, values) -> {
            Set<String> parkingLots = values.stream().map(o -> o.getParkingLot()).collect(Collectors.toSet());
            LinkedHashSet<String> resultParkingLots = new LinkedHashSet<>();
            parkingLots.forEach(o -> {
                String[] splitLots = o.split(",");
                resultParkingLots.addAll(Arrays.asList(splitLots));
            });

            String parkingLotsString = parkingLots.stream().collect(Collectors.joining(","));
            UserSpacePO spacePO = BeanConvertorUtils.map(values.get(0), UserSpacePO.class);
            spacePO.setParkingLot(parkingLotsString);
            resultList.add(spacePO);
        });
        this.saveData(resultList);
    }

    /**
     * 判断endTime是否大于等于今天  精确到天
     *
     * @param now
     * @param endDate
     * @return true:结束时间大于当前时间
     */
    private boolean judgeDateBefore(Date now, Date endDate) {
        String nowStr = DateUtil.format(now, ParkingConstants.SHORT_DATE_FORMAT);
        // 精确到天
        DateTime nowDate = DateUtil.parse(nowStr, ParkingConstants.SHORT_DATE_FORMAT);
        // 判断
        if (endDate.compareTo(nowDate) >= 0) {
            return true;
        }
        return false;
    }


	/**
	 * 根据人员工号查询拥有车位的情况
	 * @param jobNumList
	 * @return
	 */
	public List<UserSpacePO> querySpaceListByJobNum(List<String> jobNumList,Integer type) {
		return CollectionUtils.isEmpty(jobNumList) ? Collections.emptyList(): userSpaceMapper.selectList(new LambdaQueryWrapper<UserSpacePO>()
					.in(UserSpacePO::getJobNumber, jobNumList)
					.eq(UserSpacePO::getType, type)
					.ge(UserSpacePO::getEndDate, DateUtil.format(new Date(), ParkingConstants.SHORT_DATE_FORMAT))
				);
	}


	/**
	 * 把中签结果和已有车位结合起来生产新车位信息数据
	 * 确认的时候，已有的共有车位本来就快要到期了，新车位直接加定时日期就行
	 * @param lottery 
	 * @param batch
	 * @param detailList
	 * @param spaceList
	 * @param verifyList 
	 */
	public void initLotteryDetailIntoSpace(LotteryResultPO lottery, LotteryBatchPO batch, List<LotteryResultDetailPO> detailList,
			List<UserSpacePO> spaceList, List<UserVerifyPO> verifyList) {
		log.info("结果确认：batch={},detailList ={},spaceList = {}",JSON.toJSONString(batch),JSON.toJSONString(detailList),JSON.toJSONString(spaceList));
		
		List<String> pubJobNumList = detailList.stream().map(item -> item.getUserJobNumber()).collect(Collectors.toList());
		//获取本期中签人员的非摇号车库
		List<UserSpacePO> publicSpaceList = userSpaceService.querySpaceListByJobNum(pubJobNumList,UserSpaceTypeEnum.SETTING.getState());
		pubJobNumList = publicSpaceList.stream().map(space -> space.getJobNumber()).collect(Collectors.toList());
		publicSpaceList.clear();;
		//已存在需要更新的
		List<UserSpacePO> existSpaceList = new ArrayList<>();
		//新增加的
		List<UserSpacePO> initSpaceList = new ArrayList<>();
		
		//车牌信息映射成用户id--车牌集合形式
		Map<Long,List<String>> verifyMap = verifyList.stream().collect(Collectors.groupingBy(UserVerifyPO::getUserId ,Collectors.mapping(UserVerifyPO::getPlateNo, Collectors.toList())));
		verifyList.clear();
		//车位信息映射成Map形式  {jobNum:{plateNo:UserSpacePO}}
		Map<String/**工号*/, Map<String/**车牌*/, UserSpacePO>> userSpaceMap = (Map<String, Map<String, UserSpacePO>>) spaceList.stream()
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
		log.info("转换后的已存在车位信息:{}",JSON.toJSONString(userSpaceMap));
		spaceList.clear();
		//已存在的车位信息
		Map<String, UserSpacePO> parkMap = new HashMap<>();
		
		//员工所拥有的车牌列表
		List<String> plateNoList = new ArrayList<>();
		for(LotteryResultDetailPO detail : detailList) {
			
			//获取人员所拥有的车牌
			plateNoList = verifyMap.get(detail.getUserId());
			if (CollectionUtils.isEmpty(plateNoList)) {
				log.error("工号为：{}的用户无车牌",detail.getUserJobNumber());
				continue;
			}
			for (String plateNo : plateNoList) {
				if (userSpaceMap.containsKey(detail.getUserJobNumber())) { //说明当前有车位
					//获取人员车位信息
					parkMap = userSpaceMap.get(detail.getUserJobNumber());
					UserSpacePO space = initUserSpace(lottery,batch,detail,parkMap.get(plateNo),plateNo);
					if (parkMap.get(plateNo) != null && 
							parkMap.get(plateNo).getParkingLot().equals(detail.getParkingLotCode())) { //说明该车牌存在同一车库的有效车位，
						existSpaceList.add(space);
					} else {
						initSpaceList.add(space);
					}
				} else {//无车位
					UserSpacePO space = initUserSpace(lottery,batch,detail,plateNo,pubJobNumList);
					initSpaceList.add(space);
				}
			}
		}
	
		if(!CollectionUtils.isEmpty(initSpaceList)) {
			userSpaceService.saveBatch(initSpaceList);
		}
		
		if(!CollectionUtils.isEmpty(existSpaceList)) {
			userSpaceService.updateBatchById(existSpaceList);
		}
		
	}


	/**
	 * 初始化车位对象
	 * @param lottery 
	 * @param batch
	 * @param detail
	 * @param plateNo 
	 * @return
	 */
	private UserSpacePO initUserSpace(LotteryResultPO lottery, LotteryBatchPO batch, LotteryResultDetailPO detail, UserSpacePO userSpacePO, String plateNo) {
		
		//userSpacePO不为空的时候，代表该车牌已有未过期车位。需要判断车库是否是同一个,
		 if (userSpacePO != null && userSpacePO.getParkingLot().equals(detail.getParkingLotCode())) {
			//同一个车牌且同一个车库
			userSpacePO.setEndDate(batch.getValidEndDate())
			.setState(UserSpaceStateEnum.UNSYNC.getState())
			.setScheduleDate(null)
			.setUpdateTm(new Date())
			.setType(UserSpaceTypeEnum.LOTTERY.getState())
			.setBatchId(lottery.getBatchId())
			.setBatchNum(lottery.getBatchNum())
			.setRoundId(lottery.getRoundId());
			return userSpacePO;
		 } 
		 
		//两种情况：1.同一个车牌 但是不是同一个车库，2.userSpacePO为空则代表无该车牌车位
		 UserSpacePO userSpace = new UserSpacePO()
					.setCreateTm(new Date())
					.setJobNumber(detail.getUserJobNumber())
					.setName(detail.getUserName())
					.setParkingLot(detail.getParkingLotCode())
					.setState(UserSpaceStateEnum.UNSYNC.getState())
					.setUpdateTm(new Date())
					.setUserSpaceId(idWorker.nextId())
					.setEndDate(batch.getValidEndDate())
					.setStartDate(batch.getValidStartDate())
					.setPlateNo(plateNo)
					.setType(UserSpaceTypeEnum.LOTTERY.getState())
					.setBatchId(lottery.getBatchId())
					.setBatchNum(lottery.getBatchNum())
					.setRoundId(lottery.getRoundId())
					//到这一步且userSpacePO不为空，代表目前该车牌存在其它车库的车位，所以定时时间这里应该是车位生效的有效期，到时间后会有定时任务去下发闸机系统
		 			.setScheduleDate(userSpacePO == null ? null : DateUtil.format(batch.getValidStartDate(), ParkingConstants.SHORT_DATE_FORMAT) );
		 return userSpace;
			 
	}


	/**
	 * 初始化车位对象
	 * @param lottery 摇号结果
	 * @param batch	摇号批次
	 * @param detail 中签明细
	 * @param plateNo 车牌
	 * @param detail 有公共车位的人员工号
	 * @return
	 */
	private UserSpacePO initUserSpace(LotteryResultPO lottery, LotteryBatchPO batch, LotteryResultDetailPO detail,
			String plateNo,List<String> pubJobNum) {
		UserSpacePO userSpace = new UserSpacePO()
						.setCreateTm(new Date())
						.setEndDate(batch.getValidEndDate())
						.setJobNumber(detail.getUserJobNumber())
						.setName(detail.getUserName())
						.setParkingLot(detail.getParkingLotCode())
						.setStartDate(batch.getValidStartDate())
						.setState(UserSpaceStateEnum.UNSYNC.getState())
						.setUpdateTm(new Date())
						.setPlateNo(plateNo)
						//有公共车位的话，当期中签数据就在生效开始当天执行下发
						.setScheduleDate(pubJobNum.contains(detail.getUserJobNumber()) ? DateUtil.format(batch.getValidStartDate(), ParkingConstants.SHORT_DATE_FORMAT) : null )
						.setType(UserSpaceTypeEnum.LOTTERY.getState())
						.setBatchId(lottery.getBatchId())
						.setBatchNum(lottery.getBatchNum())
						.setRoundId(lottery.getRoundId())
						.setUserSpaceId(idWorker.nextId());
		return userSpace;
	}


	/**
	 * 删除time之前过期的车
	 * @param time
	 */
	public void deleteExpiredSpace(String time) {
		userSpaceMapper.delete(new LambdaUpdateWrapper<UserSpacePO>()
					.lt(UserSpacePO::getEndDate, time)
				);
	}


	/**
	 * 下发车位信息到闸机系统
	 * @param time
	 */
	public void parkingDownOnStartTtime(String time) {
		List<UserSpacePO> spaceList = userSpaceMapper.queryUnsyncBeforeScheduleDate(time);
		log.info("获取定时下发任务数据：{}",JSON.toJSONString(spaceList));
		spaceList.forEach(space -> {
			try {
				invokeCarAddService(space);
				Thread.sleep(5000);
			} catch (Exception e) {
				log.error("同步定时下发闸机数据失败：{}，失败原因：{}",JSON.toJSONString(space),e);
			}
		});
	}

	
	
	/**
	 * 同步车位信息到闸机系统
	 */
	public void syncSpace() {
		List<UserSpacePO> spaceList = userSpaceMapper.queryUnSyncData(DateUtil.format(new Date(), ParkingConstants.SHORT_DATE_FORMAT));
		log.info("获取到待同步车位信息：{}",JSON.toJSONString(spaceList));
		spaceList.forEach(space -> {
			try {
				invokeCarAddService(space);
				Thread.sleep(5000);
			} catch (Exception e) {
				log.error("同步闸机数据失败：{}，失败原因：{}",JSON.toJSONString(space),e);
			}
		});
		
	}

	
	/**
	 * 调用闸机系统把车位信息同步过去
	 * @param space
	 */
	public void invokeCarAddService(UserSpacePO space) {
		if (space == null) {
			return ;
		}
		log.info("同步车位信息：{}",JSON.toJSONString(space));
		EmployeePO employee = employeeService.queryEmployeeByJobNum(space.getJobNumber());
		if (employee == null) {
			log.info("工号：{}在员工表中不存在，跳过分配闸机权限",space.getJobNumber());
			return ;
		}
		UserSpaceDTO dto = new UserSpaceDTO();
		BeanUtils.copyProperties(space, dto);
		dto.setParkingLot(StringUtils.join( parkingLotService.queryParentListViaSelf(dto.getParkingLot()),"," ));
		ParkBaseRespBO<ParkBaseDetailRespBO> resp = parkInvokeService.replaceCarInfo(dto);

		UserSpacePO stateSpace = new UserSpacePO();
		if(resp != null && ParkingRemoteCodeEnum.RESP_SUCCESS.getState().equals(resp.getResCode()) && resp.getResult() != null &&
				ParkingRemoteCodeEnum.BUS_CODE.getState().equals(resp.getResult().getCode())) {
			stateSpace.setState(UserSpaceStateEnum.SUCCESS.getState());
			stateSpace.setFailReason("");
		} else {
			stateSpace.setState(UserSpaceStateEnum.FAIL.getState());
			stateSpace.setFailReason(resp == null ? "系统异常" : (resp.getResult() == null ? resp.getResMsg() : resp.getResult().getMessage()));
			DingAlarmUtils.alarmException("调用添加车辆接口失败:"+ JSON.toJSONString(resp));
		}
		stateSpace.setUserSpaceId(space.getUserSpaceId());
		stateSpace.setRetryNum((space.getRetryNum() == null ? 0 : space.getRetryNum()) + 1);
		log.info("车位同步id={}结果{}",stateSpace.getUserSpaceId(),JSON.toJSONString(resp));
		userSpaceMapper.updateById(stateSpace);
	}

	
	
	/**
	 * 根据工号查询车位并根据车库和有效期进行分组
	 * @param jobNum 工号
	 * @return
	 */
	public List<UserSpacePO> querySpaceGroupByExpireDate(String jobNum,Integer type) {
		return userSpaceMapper.querySpaceGroupByExpireDate(jobNum,type);
	}


	/**
	 * 根据工号和停车库查询车位信息
	 * @param jobNum 工号
	 * @param parkingLot 车库编码
	 * @return
	 */
	public List<UserSpacePO> querySpaceByJobNumAndParkLot(String jobNum, String parkingLot) {
		return userSpaceMapper.selectList(new LambdaQueryWrapper<UserSpacePO>()
				.eq(UserSpacePO::getJobNumber,jobNum)
				.eq(UserSpacePO::getParkingLot,parkingLot));
	}

	
	/**
	 * 根据批次与轮数查询对应的列表
	 * @param dto
	 * @return
	 */
	public PageResponse<LotteryResultDetailBO> pageSelectListByBatchAndRound(UserSpaceDTO dto) {
		Page<UserSpacePO> page = PageUtils.toPage(dto);
		Page<UserSpacePO> poPage = userSpaceMapper.selectPage(page, new LambdaQueryWrapper<UserSpacePO>()
				.eq(UserSpacePO::getBatchNum, dto.getBatchNum())
				.eq(UserSpacePO::getRoundId, dto.getRoundId())
				.eq(StringUtils.isNotBlank(dto.getState()), UserSpacePO::getState, dto.getState()));

		List<LotteryResultDetailBO> detailBOS = poPage.getRecords().stream().map(this::getLotteryResultDetailBOByUserSpacePO).collect(Collectors.toList());
		return PageUtils.toResponseList(page,detailBOS);
	}

	private LotteryResultDetailBO getLotteryResultDetailBOByUserSpacePO(UserSpacePO po) {
		LotteryResultDetailBO detailBO = new LotteryResultDetailBO();
		detailBO.setId(po.getUserSpaceId());
		detailBO.setParkingLotName(parkingInitService.queryParkingNameByCode(po.getParkingLot()));
		detailBO.setUserName(po.getName());
		detailBO.setUserJobNumber(po.getJobNumber());
		detailBO.setState(po.getState());
		return detailBO;
	}


	/**
	 * 根据批次查询以中签的工号
	 * @param batchId
	 * @return
	 */
	public List<String> querySpaceListByBatchId(Long batchId,Integer type) {
		return userSpaceMapper.selectList(new LambdaQueryWrapper<UserSpacePO>()
					.eq(UserSpacePO::getBatchId, batchId)
					.eq(UserSpacePO::getType, type)
				)
				.stream().map(space -> space.getJobNumber())
				.collect(Collectors.toList());
	}


	/**
	 * 根据批次号和轮次号查询未同步成功的数据量
	 * @param batchId
	 * @param roundId
	 * @return
	 */
	public long queryUnSyncListByBatch(Long batchId, Long roundId) {
		return userSpaceMapper.selectCount(new LambdaQueryWrapper<UserSpacePO>()
				.eq(UserSpacePO::getRoundId, roundId)
				.eq(UserSpacePO::getBatchId, batchId)
				.ne(UserSpacePO::getState, UserSpaceStateEnum.SUCCESS.getState())
				
			);
	}


	/**
	 * 更新员工的车位有效期到 指定日期
	 * @param jobNumber
	 * @param date
	 */
	public void updateEndDate(String jobNumber, Date date) {
		UserSpacePO space = new UserSpacePO().setEndDate(date)
				.setScheduleDate("")
				.setState(UserSpaceStateEnum.UNSYNC.getState());
		userSpaceMapper.update(space, new LambdaUpdateWrapper<UserSpacePO>().eq(UserSpacePO::getJobNumber, jobNumber));
	}


	/**
	 * 根据批次、轮次 、状态查询车位信息
	 * @param batchId 批次
	 * @param roundId 轮次
	 * @param state	状态
	 * @return
	 */
	public List<UserSpacePO> querySpaceListByBatch(Long batchId, Long roundId, String state) {
		return userSpaceMapper.selectList(new LambdaQueryWrapper<UserSpacePO>()
				.eq(UserSpacePO::getRoundId, roundId)
				.eq(UserSpacePO::getBatchId, batchId)
				.eq(UserSpacePO::getState, state));
	}


	/**
	 * 更新工号为jobNum 的type类型的起始日期和定时日期
	 * @param jobNum
	 * @param addDays
	 * @param type
	 */
	public void updateStartDate(String jobNum, Date startDate,String scheduledate, Integer type) {
		UserSpacePO space = new UserSpacePO().setStartDate(startDate)
				.setScheduleDate(scheduledate)
				.setState(UserSpaceStateEnum.UNSYNC.getState());
		userSpaceMapper.update(space, new LambdaUpdateWrapper<UserSpacePO>()
					.eq(UserSpacePO::getJobNumber, jobNum)
					.eq(UserSpacePO::getType, type)
					.ge(UserSpacePO::getEndDate, scheduledate)
				);
	}


	/**
	 * 删除用户车位
	 * @param jobNumber 工号
	 * @param parkingCode 车库
	 * @param endDate 截止日期
	 * @param type 类型
	 */
	public void deleteUserSpace(String jobNumber, String parkingCode, String endDate, Integer type) {
		userSpaceMapper.delete(new LambdaUpdateWrapper<UserSpacePO>()
				.eq(UserSpacePO::getJobNumber, jobNumber)
				.eq(UserSpacePO::getParkingLot, parkingCode)
				.eq(UserSpacePO::getType, type)
				.eq(UserSpacePO::getEndDate, endDate)
				);
		
	}
	
	/**
	 * 更改用户车牌
	 * @param vefifyList
	 */
	public void changeUserPalteNo(List<UserVerifyPO> vefifyList) {
		List<UserSpacePO> spaceList = null;
		ParkingCarQueryDTO query = new ParkingCarQueryDTO();
		//车辆查询响应
		ParkingCarQueryRespBO<ParkingCarInfoBO<ParkingYardBO>> result = null;
		
		//车辆id
		List<String> carIdList = new ArrayList<>();
		ParkingDeleteCarDTO deleteDto = new ParkingDeleteCarDTO();
		
		for(UserVerifyPO verify : vefifyList) {
			if (StringUtils.isEmpty(verify.getLastPlateNo())){
				log.info("未修改车牌，不做操作");
				//没有旧车牌就不做操作
				continue;
			}
			//查询原车牌是否有车位
			spaceList = userSpaceMapper.selectList(new LambdaQueryWrapper<UserSpacePO>()
						.eq(UserSpacePO::getPlateNo, verify.getLastPlateNo())
					);
			if (CollectionUtils.isEmpty(spaceList)) {
				continue;
			}
			log.info("车辆信息：{},原车牌车位：{}",JSON.toJSONString(verify),JSON.toJSONString(spaceList));
			for(UserSpacePO space : spaceList) {
				if(space.getEndDate().compareTo(DateUtil.beginOfDay(new Date())) <= 0) {
					//车位即将到期，不做操作
					log.info("原车位即将到期，不做操作：{}",JSON.toJSONString(space));
					continue;
				}
				
				if (space.getStartDate().compareTo(DateUtil.beginOfDay(new Date())) > 0 ) {
					log.info("车位还没开始生效：{}",JSON.toJSONString(space));
					//车位还没开始生效。如果闸机权限已下发，则删除闸机权限， (删除原车位，添加新车位)---->直接把车牌换掉
					if(UserSpaceStateEnum.SUCCESS.getState().equals(space.getState())) {
						//闸机权限已下发
						query.setCarOwner(space.getName());
						query.setLicensePlate(space.getPlateNo());
						result = parkInvokeService.queryCarInfo(query);
						log.info("查询车辆信息：{},响应:{}",JSON.toJSONString(query),JSON.toJSONString(result) );
						if (result != null && !CollectionUtils.isEmpty(result.getData())) {
							carIdList.clear();
							carIdList = result.getData().stream().map(item -> item.getId()).collect(Collectors.toList());
							deleteDto.setId(carIdList);
							ParkBaseRespBO<ParkBaseDetailRespBO> response = parkInvokeService.deleteCarInfo(deleteDto);
							log.info("删除车辆信息：{},响应:{}",JSON.toJSONString(deleteDto),JSON.toJSONString(response) );
						}
					}
					space.setPlateNo(verify.getPlateNo());
					space.setState(UserSpaceStateEnum.UNSYNC.getState());
					space.setScheduleDate("");
					userSpaceMapper.updateById(space);
					log.info("直接替换原车牌车位信息：{}",JSON.toJSONString(space));
				}
				
				if (space.getStartDate().compareTo(DateUtil.beginOfDay(new Date())) <= 0 ) {
					log.info("车位已经生效：{}",JSON.toJSONString(space));
					//添加新车位
					UserSpacePO newSpace = new UserSpacePO();
					BeanUtil.copyProperties(space, newSpace);
					newSpace.setUserSpaceId(idWorker.nextId())
							.setState(UserSpaceStateEnum.UNSYNC.getState())
							.setPlateNo(verify.getPlateNo())
							.setScheduleDate("")
							.setFailReason("")
							.setCreateTm(new Date())
							.setUpdateTm(new Date())
							.setStartDate(DateUtils.addDays(new Date(), 1));//从第二天生效		
					userSpaceMapper.insert(newSpace);
					log.info("添加新车牌车位信息：{}",JSON.toJSONString(newSpace));
					//车位已经生效了，把原车位改成今天到期
					space.setEndDate(new Date())
							.setState(UserSpaceStateEnum.UNSYNC.getState())
							.setScheduleDate("");
					userSpaceMapper.updateById(space);
					log.info("更新原车牌车位信息：{}",JSON.toJSONString(space));
				}
				
			};
		}
	}
}

