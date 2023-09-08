package com.cf.parking.services.facade.impl;


import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryResultMapper;
import com.cf.parking.dao.po.*;
import com.cf.parking.facade.bo.LotteryBatchBO;
import com.cf.parking.facade.bo.LotteryResultBO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.dto.LotteryResultDTO;
import com.cf.parking.facade.facade.LotteryResultFacade;
import com.cf.parking.services.enums.EnableStateEnum;
import com.cf.parking.services.enums.RuleAssignTypeEnum;
import com.cf.parking.services.service.*;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

/**
 * 摇号结果Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Service
@Slf4j
public class LotteryResultFacadeImpl implements LotteryResultFacade
{
    @Resource
    private LotteryResultMapper lotteryResultMapper;

    @Resource
    private LotteryRuleRoundService lotteryRuleRoundService;
    
    @Resource
    private LotteryBatchService lotteryBatchService;
    
    @Resource
    private LotteryService lotteryService;
    
    @Resource
    private LotteryRuleAssignService lotteryRuleAssignService;
    
    @Resource
    private ParkingLotService parkingLotService;
    
    @Resource
    private LotteryApplyRecordService lotteryApplyRecordService;

    @Resource
	private LotteryResultDetailService lotteryResultDetailService;
    
    
	@Override
	public void lottery(Long id) {
		
		log.info("开始摇号：{}",id);
		LotteryResultPO lottery = lotteryResultMapper.selectById(id);
		AssertUtil.checkNull(lottery, "数据不存在");
		LotteryRuleRoundPO round = lotteryRuleRoundService.getById(lottery.getRoundId());
		AssertUtil.checkNull(round, "轮次数据不存在");
		AssertUtil.checkTrue(EnableStateEnum.ENABLE.getState().equals(round.getState()),"摇号轮次未启用");
		LotteryBatchPO batch = lotteryBatchService.getById(lottery.getBatchId());
		AssertUtil.checkNull(batch, "批次数据不存在");
		//获取停车场信息
		String parklotCode = round.getParkingLotCode();
		List<String> parkingList = Arrays.asList(parklotCode.split(","));
		List<ParkingLotPO> parkingLotList = parkingLotService.selectParkingLotByCodes(parkingList);
		log.info("初步获取到摇号停车场信息：{}",JSON.toJSONString(parkingLotList));
		AssertUtil.checkTrue(!CollectionUtils.isEmpty(parkingLotList), "停车场信息不存在，请检查设置");
		parkingLotList = parkingLotList.stream().filter(item -> EnableStateEnum.ENABLE.getState().equals(item.getType())).collect(Collectors.toList());
		AssertUtil.checkTrue(!CollectionUtils.isEmpty(parkingLotList), "停车场的配置为不可参加摇号，请检查设置");
		//获取报名的人员,要符合个人中心的车库在这次摇号的车库中这个条件
		List<LotteryApplyRecordPO> applyList = lotteryApplyRecordService.queryLotteryApplyList(lottery.getBatchId(),parkingList);
		log.info("获取到报名摇号的人员信息：{}",JSON.toJSONString(applyList));
		parkingLotList.forEach(parkLot -> {
			//获取该车库报名人员的工号
			List<String> jobNumberList = applyList.stream().filter(item-> item.getParkingLotCode().equals(parkLot.getRegionCode())).map(item -> item.getJobNumber()).collect(Collectors.toList());
			log.info("获取到报名停车场：{}的人员工号",parkLot.getRegionCode(),JSON.toJSONString(jobNumberList));
			
			//获取配置人员数据
			List<LotteryRuleAssignPO> empList = lotteryRuleAssignService.queryRuleAssignListByJobNumber(jobNumberList,RuleAssignTypeEnum.EMPLOYEE.getState());
			//获取配置部门的数据
			List<LotteryRuleAssignPO> deptList = lotteryRuleAssignService.queryRuleAssignListByJobNumber(jobNumberList,RuleAssignTypeEnum.DEPARMENT.getState());
			
			if(!CollectionUtils.isEmpty(empList)) {
				//把数据映射成工号-->停车集合的形式
				Map<String,List<String>> userParking = empList.stream().collect(Collectors.groupingBy(LotteryRuleAssignPO::getCode,Collectors.mapping(LotteryRuleAssignPO::getParkingLotCode, Collectors.toList())));
				//userParking.
			}
			
		});
		lotteryService.doLottery(batch,lottery,round);
	}

	/**
	 * 查询摇号结果列表
	 * @param dto
	 * @return
	 */
	@Override
	public PageResponse<LotteryResultBO> getLotteryResultList(LotteryResultDTO dto) {
		Page<LotteryResultPO> page = PageUtils.toPage(dto);

		LambdaQueryWrapper<LotteryResultPO> queryWrapper = new LambdaQueryWrapper<LotteryResultPO>()
				.le(!ObjectUtils.isEmpty(dto.getEndDate()), LotteryResultPO::getBatchNum, dto.getEndDate())
				.ge(!ObjectUtils.isEmpty(dto.getStartDate()) , LotteryResultPO::getBatchNum, dto.getStartDate())
				.like(!ObjectUtils.isEmpty(dto.getRoundId()) , LotteryResultPO::getRoundId, dto.getRoundId())
				.eq(StringUtils.isNotEmpty(dto.getState()), LotteryResultPO::getState, dto.getState())
				.ne(LotteryResultPO::getState,"5")
				.orderByDesc(LotteryResultPO::getBatchNum);

		Page<LotteryResultPO> poPage = lotteryResultMapper.selectPage(page, queryWrapper);
		List<LotteryResultBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryResultBO.class);
		return PageUtils.toResponseList(page,boList);
	}

	/**
	 * 结果归档(当本期所有记录均归档完成后，将该批次的状态更改为已结束)
	 * @param id
	 * @return
	 */
	@Transactional(rollbackFor = Exception.class)
	@Override
	public Integer archive(Long id) {

		//1.更新状态为已归档
		LotteryResultPO po = lotteryResultMapper.selectById(id);
		po.setState("5");
		int result = lotteryResultMapper.updateById(po);

		//2.查看相同期号下是否还有其他未归档的记录。如果没有，将该期摇号批次表状态变为已结束
		List<LotteryResultPO> lotteryResultPOList = lotteryResultMapper.selectList(new LambdaQueryWrapper<LotteryResultPO>()
				.eq(LotteryResultPO::getBatchNum, po.getBatchNum())
				.ne(LotteryResultPO::getState, "5"));

		if (CollectionUtils.isEmpty(lotteryResultPOList)){
			result = lotteryBatchService.archive(po.getBatchId());
		}
		return result;
	}

	/**
	 * 摇号结果分页查询
	 * @param dto
	 * @return
	 */
	@Override
	public PageResponse<LotteryResultDetailBO> lotteryResult(LotteryResultDTO dto) {
		Page<LotteryResultDetailPO> page = PageUtils.toPage(dto);
		return lotteryResultDetailService.selectDetailListByResultId(page, dto.getId());
	}

}
