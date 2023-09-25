package com.cf.parking.services.service;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.bo.ParkBaseDetailRespBO;
import com.cf.parking.facade.bo.ParkBaseRespBO;
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
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.dao.po.UserSpacePO;
import com.cf.parking.dao.po.UserVerifyPO;
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
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
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
	 * @param lottery 
	 * @param batch
	 * @param detailList
	 * @param spaceList
	 * @param verifyList 
	 */
	public void initLotteryDetailIntoSpace(LotteryResultPO lottery, LotteryBatchPO batch, List<LotteryResultDetailPO> detailList,
			List<UserSpacePO> spaceList, List<UserVerifyPO> verifyList) {
		log.info("结果确认：batch={},detailList ={},spaceList = {}",JSON.toJSONString(batch),JSON.toJSONString(detailList),JSON.toJSONString(spaceList));
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
					UserSpacePO space = initUserSpace(lottery,batch,detail,plateNo);
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
	 * @param lottery 
	 * @param batch
	 * @param detail
	 * @return
	 */
	private UserSpacePO initUserSpace(LotteryResultPO lottery, LotteryBatchPO batch, LotteryResultDetailPO detail, String plateNo) {
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
						.setScheduleDate(null)
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
		List<UserSpacePO> spaceList = userSpaceMapper.selectList(new LambdaQueryWrapper<UserSpacePO>()
					.le(UserSpacePO::getScheduleDate, time)
					.ne(UserSpacePO::getState, UserSpaceStateEnum.SUCCESS.getState())
				);
		log.info("获取定时任务列表：{}",JSON.toJSONString(spaceList));
		UserSpaceDTO dto = new UserSpaceDTO();
		spaceList.forEach(space->{
			try {
				invokeCarAddService(space);
				Thread.sleep(5000);
			} catch (Exception e) {
				log.info("定时任务车位同步id={}出错{}",space.getUserSpaceId(),e);
			}
		});
	}

	
	
	/**
	 * 同步车位信息到闸机系统
	 */
	public void syncSpace() {
		UserSpacePO space = userSpaceMapper.selectOne(new LambdaQueryWrapper<UserSpacePO>()
					.ne(UserSpacePO::getState, UserSpaceStateEnum.SUCCESS.getState())
					.ge(UserSpacePO::getEndDate, DateUtil.format(new Date(), ParkingConstants.SHORT_DATE_FORMAT))
					.last(" and ifnull(schedule_date,'') = '' limit 1 ")
				);
		log.info("获取到待同步车位信息：{}",JSON.toJSONString(space));
		if (space == null) {
			return ;
		}
		
		invokeCarAddService(space);
	}

	
	/**
	 * 调用闸机系统把车位信息同步过去
	 * @param space
	 */
	public void invokeCarAddService(UserSpacePO space) {
		log.info("同步车位信息：{}",JSON.toJSONString(space));
		UserSpaceDTO dto = new UserSpaceDTO();
		BeanUtils.copyProperties(space, dto);
		dto.setParkingLot(StringUtils.join( parkingLotService.queryParentListViaSelf(dto.getParkingLot()),"," ));
		ParkBaseRespBO<ParkBaseDetailRespBO> resp = parkInvokeService.replaceCarInfo(dto);

		UserSpacePO stateSpace = new UserSpacePO();
		if(resp != null && ParkingRemoteCodeEnum.RESP_SUCCESS.getState().equals(resp.getResCode()) && resp.getResult() != null &&
				ParkingRemoteCodeEnum.BUS_CODE.getState().equals(resp.getResult().getCode())) {
			stateSpace.setState(UserSpaceStateEnum.SUCCESS.getState());
		} else {
			stateSpace.setState(UserSpaceStateEnum.FAIL.getState());
			stateSpace.setFailReason(resp == null ? "系统异常" : (resp.getResult() == null ? resp.getResMsg() : resp.getResult().getMessage()));
		}
		stateSpace.setUserSpaceId(space.getUserSpaceId());
		log.info("车位同步id={}结果{}",stateSpace.getUserSpaceId(),JSON.toJSONString(resp));
		userSpaceMapper.updateById(stateSpace);
	}

	/**
	 * 根据工号查询车位并根据车库和有效期进行分组
	 * @param jobNum 工号
	 * @return
	 */
	public List<UserSpacePO> querySpaceGroupByExpireDate(String jobNum) {
		return userSpaceMapper.querySpaceGroupByExpireDate(jobNum);
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
		detailBO.setParkingLotName(parkingLotService.selectParkingLotByCode(po.getParkingLot()).getRegion());
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
}

