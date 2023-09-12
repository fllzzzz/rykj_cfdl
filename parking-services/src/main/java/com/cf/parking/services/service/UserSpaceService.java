package com.cf.parking.services.service;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import com.cf.parking.facade.bo.UserSpaceBO;
import com.cf.parking.services.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.UserSpaceMapper;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.dao.po.UserSpacePO;
import com.cf.parking.dao.po.UserVerifyPO;
import com.cf.parking.facade.dto.Carmanagement;
import com.cf.parking.facade.dto.UserSpaceDTO;
import com.cf.parking.facade.dto.UserSpaceFuncTimeDTO;
import com.cf.parking.facade.dto.UserSpacePageDTO;
import com.cf.parking.facade.dto.UserSpaceValidityDTO;
import com.cf.parking.services.enums.UserSpaceStateEnum;
import com.cf.parking.services.integration.ParkInvokeService;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.BeansException;
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
    private final String DATE_FORMAT_STR = "yyyy-MM-dd";

    @Resource
    private UserSpaceService userSpaceService;
    @Resource
    private UserSpaceMapper userSpaceMapper;

    @Resource
    private ParkInvokeService parkInvokeService;
    
    
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
            userSpacePO.setStartDate(DateUtil.parse(startTime, DATE_FORMAT_STR));
            DateTime endDate = DateUtil.parse(endTime, DATE_FORMAT_STR);
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

    // 四楼
    //  四楼，五楼，产品

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
        String nowStr = DateUtil.format(now, DATE_FORMAT_STR);
        // 精确到天
        DateTime nowDate = DateUtil.parse(nowStr, DATE_FORMAT_STR);
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
	public List<UserSpacePO> querySpaceListByJobNum(List<String> jobNumList) {
		return CollectionUtils.isEmpty(jobNumList) ? null : userSpaceMapper.selectList(new LambdaQueryWrapper<UserSpacePO>()
					.eq(UserSpacePO::getJobNumber, jobNumList)
					.ge(UserSpacePO::getEndDate, DateUtil.format(DateUtil.beginOfDay(new Date()), "yyyy-MM-dd HH:mm:ss"))
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
		verifyList.clear();;
		//车位信息映射成Map形式  {jobNum:{plateNo:UserSpacePO}}
		Map<String/**工号*/, Map<String/**车牌*/, UserSpacePO>> userSpaceMap = (Map<String, Map<String, UserSpacePO>>) spaceList.stream()
		        .collect(Collectors.toMap(UserSpacePO::getJobNumber, // 作为外层Map的键
		                userSpacePO -> {
		                    Map<String, UserSpacePO> innerMap = new HashMap<>();
		                    innerMap.put(userSpacePO.getPlateNo(), userSpacePO); // 使用地址作为内层Map的键
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
			userSpaceService.updateBatchById(initSpaceList);
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
		//userSpacePO不为空的时候，代表该车牌已有未过期车位。需要判断车库是否是同一个
		 if (userSpacePO != null && userSpacePO.getParkingLot().equals(detail.getParkingLotCode())) {
			//同一个车牌且同一个车库
			userSpacePO.setEndDate(batch.getValidEndDate());
			userSpacePO.setState(UserSpaceStateEnum.UNSYNC.getState());
			userSpacePO.setScheduleDate(null);
			userSpacePO.setBatchId(lottery.getBatchId());
			userSpacePO.setBatchNum(lottery.getBatchNum());
			userSpacePO.setRoundId(lottery.getRoundId());
			return userSpacePO;
		 } 
			 //同一个车牌 但是不是同一个车库
			 UserSpacePO userSpace = new UserSpacePO()
						.setCreateTm(new Date())
						.setJobNumber(detail.getUserJobNumber())
						.setName(detail.getUserName())
						.setParkingLot(detail.getParkingLotCode())
						.setState(UserSpaceStateEnum.UNSYNC.getState())
						.setUpdateTm(new Date())
						.setUserSpaceId(IdWorker.getId())
						.setEndDate(batch.getValidEndDate())
						.setStartDate(batch.getValidStartDate())
						.setPlateNo(plateNo)
						.setBatchId(lottery.getBatchId())
						.setBatchNum(lottery.getBatchNum())
						.setRoundId(lottery.getRoundId())
						//到这一步且userSpacePO不为空，代表目前该车牌存在其它车库的车位，所以定时时间这里应该是车位生效的有效期，到时间后会有定时任务去下发闸机系统
			 			.setScheduleDate(userSpacePO == null ?  null : DateUtil.format(batch.getValidStartDate(), DATE_FORMAT_STR) );
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
						.setBatchId(lottery.getBatchId())
						.setBatchNum(lottery.getBatchNum())
						.setRoundId(lottery.getRoundId())
						.setUserSpaceId(IdWorker.getId());
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
				BeanUtils.copyProperties(space, dto);;
				boolean flag = parkInvokeService.replaceCarInfo(dto);
				log.info("定时任务车位同步id={}结果{}",space.getUserSpaceId(),flag);
				space.setState(flag ? UserSpaceStateEnum.SUCCESS.getState() : UserSpaceStateEnum.FAIL.getState());
				userSpaceMapper.updateById(space);
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
					.le(UserSpacePO::getEndDate, DateUtil.format(new Date(), "yyyy-MM-dd"))
					.isNull(UserSpacePO::getScheduleDate)
					.orderByAsc(UserSpacePO::getUserSpaceId)
					.last(" limit 1 ")
				);
		log.info("获取到待同步车位信息：{}",JSON.toJSONString(space));
		if (space == null) {
			return ;
		}
		
		UserSpaceDTO dto = new UserSpaceDTO();
		BeanUtils.copyProperties(space, dto);;
		boolean flag = parkInvokeService.replaceCarInfo(dto);
		log.info("车位同步id={}结果{}",space.getUserSpaceId(),flag);;
		space.setState(flag ? UserSpaceStateEnum.SUCCESS.getState() : UserSpaceStateEnum.FAIL.getState());
		userSpaceMapper.updateById(space);
	}


	public List<UserSpacePO> querySpaceGroupByExpireDate(String outJobNum) {
		return userSpaceMapper.querySpaceGroupByExpireDate(outJobNum);
	}


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
	public PageResponse<UserSpaceBO> pageSelectListByBatchAndRound(UserSpaceDTO dto) {
		Page<UserSpacePO> page = PageUtils.toPage(dto);
		Page<UserSpacePO> poPage = userSpaceMapper.selectPage(page, new LambdaQueryWrapper<UserSpacePO>()
				.eq(UserSpacePO::getBatchNum, dto.getBatchNum())
				.eq(UserSpacePO::getRoundId, dto.getRoundId())
				.eq(StringUtils.isNotBlank(dto.getState()), UserSpacePO::getState, dto.getState()));

		List<UserSpaceBO> userSpaceBOS = BeanConvertorUtils.copyList(poPage.getRecords(), UserSpaceBO.class);
		return PageUtils.toResponseList(page,userSpaceBOS);
	}


	/**
	 * 根据批次查询以中签的工号
	 * @param batchId
	 * @return
	 */
	public List<String> querySpaceListByBatchId(Long batchId) {
		return userSpaceMapper.selectList(new LambdaQueryWrapper<UserSpacePO>()
					.eq(UserSpacePO::getBatchId, batchId))
				.stream().map(space -> space.getJobNumber())
				.collect(Collectors.toList());
	}
}

