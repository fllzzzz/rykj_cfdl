package com.cf.parking.services.facade.impl;
import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryResultMapper;
import com.cf.parking.dao.po.LotteryApplyRecordPO;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.dao.po.LotteryRuleRoundPO;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.dao.po.UserPO;
import com.cf.parking.dao.po.UserProfilePO;
import com.cf.parking.dao.po.UserSpacePO;
import com.cf.parking.dao.po.UserVerifyPO;
import com.cf.parking.facade.bo.LotteryResultBO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.dto.LotteryResultDTO;
import com.cf.parking.facade.dto.TextMessageDTO;
import com.cf.parking.facade.dto.UserSpaceDTO;
import com.cf.parking.facade.facade.DingTalkMessageFacade;
import com.cf.parking.facade.facade.LotteryResultFacade;
import com.cf.parking.services.constant.ParkingConstants;
import com.cf.parking.services.enums.EnableStateEnum;
import com.cf.parking.services.enums.LotteryEnableStateEnum;
import com.cf.parking.services.enums.LotteryResultStateEnum;
import com.cf.parking.services.enums.UserSpaceStateEnum;
import com.cf.parking.services.enums.UserSpaceTypeEnum;
import com.cf.parking.services.integration.ParkInvokeService;
import com.cf.parking.services.service.DepartmentService;
import com.cf.parking.services.service.EmployeeService;
import com.cf.parking.services.service.LotteryApplyRecordService;
import com.cf.parking.services.service.LotteryBatchService;
import com.cf.parking.services.service.LotteryBlackListService;
import com.cf.parking.services.service.LotteryDealService;
import com.cf.parking.services.service.LotteryResultDetailService;
import com.cf.parking.services.service.LotteryRuleAssignService;
import com.cf.parking.services.service.LotteryRuleRoundService;
import com.cf.parking.services.service.ParkingLotService;
import com.cf.parking.services.service.UserProfileService;
import com.cf.parking.services.service.UserService;
import com.cf.parking.services.service.UserSpaceService;
import com.cf.parking.services.service.UserVerifyService;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.exception.BusinessException;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import cn.hutool.core.date.DateUtil;
import lombok.extern.slf4j.Slf4j;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.lang3.StringUtils;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
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
    private LotteryDealService lotteryDealService;
    
    @Resource
    private LotteryRuleAssignService lotteryRuleAssignService;
    
    @Resource
    private ParkingLotService parkingLotService;
    
    @Resource
    private LotteryApplyRecordService lotteryApplyRecordService;
    
    @Resource
    private EmployeeService employeeService;
    
    
    @Resource
    private LotteryBlackListService lotteryBlackListService;
    
    @Resource
    private LotteryResultDetailService lotteryResultDetailService;
    
    
    @Resource
    private DepartmentService departmentService;
    
    @Resource
    private UserSpaceService userSpaceService;
    
    @Resource
    private UserVerifyService userVerifyService;
    
    @Resource
    private ThreadPoolTaskExecutor asyncExecutor;
    
    @Resource
    private ParkInvokeService parkInvokeService;

    @Resource
    private UserProfileService userProfileService;

    private final String  message = "恭喜您抽中%s车位,有效期为%s~%s";
    
    @Resource
    private DingTalkMessageFacade dingTalkMessageFacade;
    
    @Resource
    private UserService userService;
    
    
    
    @Transactional(rollbackFor = Exception.class)
	@Override
	public void lottery(Long id) {
		
		log.info("开始摇号：{}",id);
		LotteryResultPO lottery = lotteryResultMapper.selectById(id);
		AssertUtil.checkNull(lottery, "数据不存在");
		AssertUtil.checkTrue(LotteryResultStateEnum.UNLOTTERY.getState().equals(lottery.getState()),"状态不为待摇号状态，不能摇号");
		LotteryRuleRoundPO round = lotteryRuleRoundService.getById(lottery.getRoundId());
		AssertUtil.checkNull(round, "轮次数据不存在");
		AssertUtil.checkTrue(EnableStateEnum.ENABLE.getState().equals(round.getState()),"摇号轮次未启用");
		LotteryBatchPO batch = lotteryBatchService.getById(lottery.getBatchId());
		AssertUtil.checkNull(batch, "批次数据不存在");
		//AssertUtil.checkTrue(batch.getApplyEndTime().compareTo(new Date()) < 0, "当前还处于申请日期内，不能进行摇号操作");

		//防并发
		int num = lotteryResultMapper.updateByState(id,LotteryResultStateEnum.UNLOTTERY.getState(),LotteryResultStateEnum.UNPUBLIC.getState());
		AssertUtil.checkTrue(num == 1, "状态已变更，请刷新重试");
		
		//获取停车场信息
		String parklotCode = round.getParkingLotCode();
		ParkingLotPO parkingLot = parkingLotService.selectParkingLotByCode(parklotCode);
		log.info("初步获取到摇号停车场信息：{}",JSON.toJSONString(parkingLot));
		AssertUtil.checkNull(parkingLot, "停车场信息不存在，请检查设置");
		AssertUtil.checkTrue(LotteryEnableStateEnum.ENABLE.getState().equals(parkingLot.getType()), "停车场的配置为不可参加摇号，请检查设置");
		//获取报名的人员,要符合个人中心的车库在这次摇号的车库中这个条件
		List<LotteryApplyRecordPO> applyList = lotteryApplyRecordService.queryLotteryApplyList(lottery.getBatchId(),null);
		log.info("获取到报名摇号的人员信息：{}",JSON.toJSONString(applyList));
		if (CollectionUtils.isEmpty(applyList)) {
			log.info("无报名摇号人员，程序退出");
			throw new BusinessException("没有人员报名摇号");
		}
		//获取全部报名人员的工号
		List<String> jobNumberList = applyList.stream().map(item -> item.getJobNumber()).collect(Collectors.toList());
		//根据工号获取在职员工信息
		List<String> employeeJobNumList = employeeService.queryEmployeeListByJobNum(jobNumberList);
		//获取黑名单用户
		List<String> blackJobNumList = lotteryBlackListService.queryBlackList();
		log.info("黑名单信息：{}",blackJobNumList);
		//过滤掉离职人员和黑名单人员
		applyList = applyList.stream().filter(item -> (employeeJobNumList.contains(item.getJobNumber()) && !blackJobNumList.contains(item.getJobNumber()) ) ).collect(Collectors.toList());
		log.info("过滤掉离职和黑名单后的报名摇号的人员信息：{}",JSON.toJSONString(applyList));
		//查询当前批次已中签的人员工号
		List<String> spaceJobNumList = lotteryResultDetailService.querySpaceListByBatchId(lottery.getBatchId());
		//过滤掉已中奖的
		applyList = applyList.stream().filter(item -> (!spaceJobNumList.contains(item.getJobNumber())  ) ).collect(Collectors.toList());
		AssertUtil.checkTrue(!CollectionUtils.isEmpty(applyList),"过滤后参与摇号人员为空");
		
		//释放数据
		jobNumberList.clear();
		blackJobNumList.clear();
		employeeJobNumList.clear();
		
		applyList = lotteryRuleAssignService.dealApplyByRule(lottery.getBatchId(),applyList,round.getId());
		lotteryDealService.doLottery(batch,lottery,parkingLot,applyList);
		
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
				.eq(!ObjectUtils.isEmpty(dto.getRoundId()) , LotteryResultPO::getRoundId, dto.getRoundId())
				.eq(StringUtils.isNotEmpty(dto.getState()), LotteryResultPO::getState, dto.getState())
				.ne(LotteryResultPO::getState,LotteryResultStateEnum.HAVE_ARCHIVED.getState())
				.orderByDesc(LotteryResultPO::getBatchNum);

		Page<LotteryResultPO> poPage = lotteryResultMapper.selectPage(page, queryWrapper);
		List<LotteryResultBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryResultBO.class);
		//roundName设置
		boList.forEach(lotteryResultBO -> {
			LotteryRuleRoundPO roundPO = lotteryRuleRoundService.getById(lotteryResultBO.getRoundId());
			if (null != roundPO ){
				lotteryResultBO.setRoundName(roundPO.getName());
			}
		});

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
		po.setState(LotteryResultStateEnum.HAVE_ARCHIVED.getState());
		int result = lotteryResultMapper.updateById(po);

		//2.查看相同期号下是否还有其他未归档的记录。如果没有，将该期摇号批次表状态变为已结束
		List<LotteryResultPO> lotteryResultPOList = lotteryResultMapper.selectList(new LambdaQueryWrapper<LotteryResultPO>()
				.eq(LotteryResultPO::getBatchNum, po.getBatchNum())
				.ne(LotteryResultPO::getState, LotteryResultStateEnum.HAVE_ARCHIVED.getState()));

		if (CollectionUtils.isEmpty(lotteryResultPOList)){
			lotteryBatchService.endByBatchId(po.getBatchId());
		}
		return result;
	}

	/**
	 * 摇号结果确认
	 */
	@Transactional(rollbackFor = Exception.class)
	@Override
	public void confirm(Long id) {
		LotteryResultPO lottery = lotteryResultMapper.selectById(id);
		AssertUtil.checkNull(lottery, "数据不存在");
		AssertUtil.checkTrue(LotteryResultStateEnum.UNCONFIRM.getState().equals(lottery.getState()),"状态已变更，请刷新重试");
		LotteryBatchPO batch = lotteryBatchService.getById(lottery.getBatchId());
		AssertUtil.checkNull(batch, "批次数据不存在");
		int num = lotteryResultMapper.updateByState(id,LotteryResultStateEnum.UNCONFIRM.getState(),LotteryResultStateEnum.CONFIRM_IN_PROCESS.getState());
		if (num < 1) {
			throw new BusinessException("状态已变更，请刷新重试");
		}
		//查询中签明细列表数据
		List<LotteryResultDetailPO> detailList = lotteryResultDetailService.queryDetailListByResultId(id);
		AssertUtil.checkTrue(!CollectionUtils.isEmpty(detailList), "无摇号中签数据");
		List<String> jobNumList = detailList.stream().map(item -> item.getUserJobNumber()).collect(Collectors.toList());
		//根据本期人员的工号获取其中仍有车位的人员
		List<UserSpacePO> spaceList = userSpaceService.querySpaceListByJobNum(jobNumList,UserSpaceTypeEnum.LOTTERY.getState());
		jobNumList.clear();
		//查询车牌信息;
		List<Long> userIdList = detailList.stream().map(item -> item.getUserId()).collect(Collectors.toList());
		List<UserVerifyPO> verifyList = userVerifyService.queryVerifyListByUserIdList(userIdList);
		userIdList.clear();
		userSpaceService.initLotteryDetailIntoSpace(lottery,batch,detailList,spaceList,verifyList);
		
	}

	@Override
	public PageResponse<LotteryResultDetailBO> lotteryResult(LotteryResultDTO dto) {
        Page<LotteryResultDetailPO> page = PageUtils.toPage(dto);
		PageResponse<LotteryResultDetailBO> boPageResponse = lotteryResultDetailService.selectDetailListByResultId(page, dto.getId());
		//设置停车场名称
		boPageResponse.getList().forEach(x->{
			ParkingLotPO parkingLotPO = parkingLotService.selectParkingLotByCode(x.getParkingLotCode());
			x.setParkingLotName(parkingLotPO.getRegion());
		});
		return boPageResponse;
	}


	
	/**
	 * 确认结果查询（用户车位表中的记录）
	 * @param dto
	 * @return
	 */
	@Override
	public PageResponse<LotteryResultDetailBO> confirmResult(UserSpaceDTO dto) {
		return userSpaceService.pageSelectListByBatchAndRound(dto);
	}


	@Override
	public void syncRetry(Long batchId, Long roundId) {
		List<UserSpacePO> failList = userSpaceService.querySpaceListByBatch(batchId, roundId,UserSpaceStateEnum.FAIL.getState());
		if (CollectionUtils.isEmpty(failList)){
			return ;
		}
		asyncExecutor.execute(new Runnable() {
			@Override
			public void run() {
				failList.forEach(space -> {
					try {
						userSpaceService.invokeCarAddService(space);
						Thread.sleep(5000);
					} catch (Exception e) {
						log.error("一键同步过程中车位={}报错={}", JSON.toJSONString(space),e);
					}
				});
			}
		});		
	}


	@Transactional(rollbackFor = Exception.class)
	@Override
	public void notify(Long id) {
		LotteryResultPO lottery = lotteryResultMapper.selectById(id);
		AssertUtil.checkNull(lottery, "数据不存在");
		AssertUtil.checkTrue(LotteryResultStateEnum.UNPUBLIC.getState().equals(lottery.getState()),"数据不为待发布状态，不能发布");
		int num = lotteryResultMapper.updateByState(id,LotteryResultStateEnum.UNPUBLIC.getState(),LotteryResultStateEnum.UNCONFIRM.getState());
		if (num < 1) {
			throw new BusinessException("状态已变更，请刷新重试");
		}
		LotteryBatchPO batch = lotteryBatchService.getById(lottery.getBatchId());
		AssertUtil.checkNull(batch, "批次数据不存在");
		
		LotteryRuleRoundPO round = lotteryRuleRoundService.getById(lottery.getRoundId());
		AssertUtil.checkNull(round, "轮次数据不存在");
		ParkingLotPO parking = parkingLotService.selectParkingLotByCode(round.getParkingLotCode());
		
		//未发布的数量
		long unfinshNum = lotteryResultMapper.selectCount(new LambdaQueryWrapper<LotteryResultPO>()
					.eq(LotteryResultPO::getBatchId, lottery.getBatchId())
					.in(LotteryResultPO::getState, Arrays.asList( 
							LotteryResultStateEnum.UNPUBLIC.getState()))
				);
		//查询中奖数据
		List<LotteryResultDetailPO> detailList = lotteryResultDetailService.queryDetailListByResultId(id);
		if (!CollectionUtils.isEmpty(detailList)) {
			//获取用户ID
			List<Long> userIdList = detailList.stream().map(item -> item.getUserId()).collect(Collectors.toList());
			List<UserPO> userList = userService.getUserByUserIdList(userIdList);		
			
			List<String> openIdList = userList.stream().map(item -> item.getOpenId()).collect(Collectors.toList());
			
			log.info("中奖人员工号：{},摇中停车场：{}",JSON.toJSONString(openIdList),JSON.toJSONString(parking));
			userList.clear();
			List<TextMessageDTO> messageList = new ArrayList<>();
			TextMessageDTO messageDto = new TextMessageDTO()
					.setOpenIdList(openIdList)
					.setMessage(String.format(message, parking.getRegion(),DateUtil.format(batch.getValidStartDate(),ParkingConstants.SHORT_DATE_FORMAT),
							DateUtil.format(batch.getValidEndDate(),ParkingConstants.SHORT_DATE_FORMAT)));
			messageList.add(messageDto);
			dingTalkMessageFacade.asyncSendBatchText(messageList);
			//更新默认停车场
			userProfileService.batchSetDefaultParkingLotByUserIds(userIdList,parking.getRegion());
			//更新申请记录表的摇号结果
			lotteryApplyRecordService.updateResultByUserId(userIdList,parking.getRegion(),batch.getId());
		}
		detailList.clear();;
		
		
		if (unfinshNum == 0) {
			//申请列表
			List<LotteryApplyRecordPO> applyList = lotteryApplyRecordService.queryLotteryApplyList(lottery.getBatchId(), null);
			List<String> applyIdList = applyList.stream().map(item -> item.getJobNumber()).collect(Collectors.toList());
			applyList.clear();
			//中奖人员
			List<String> spaceList = lotteryResultDetailService.querySpaceListByBatchId(lottery.getBatchId());
			applyIdList.removeAll(spaceList);
			spaceList.clear();
			log.info("未中奖人员工号：{}",JSON.toJSONString(applyIdList));
			//查询未中奖人员信息
			List<UserProfilePO> profileList = userProfileService.getProfileListByJobNumList(applyIdList);
			applyIdList.clear();
			//查询openid
			List<Long> userIdList = profileList.stream().map(item -> item.getUserId()).collect(Collectors.toList());
			profileList.clear();
			List<UserPO> userList = userService.getUserByUserIdList(userIdList);
			List<String> openIdList = userList.stream().map(item -> item.getOpenId()).collect(Collectors.toList());
			userList.clear();;
			List<TextMessageDTO> messageList = new ArrayList<>();
			TextMessageDTO messageDto = new TextMessageDTO()
					.setOpenIdList(openIdList)
					.setMessage("很遗憾，您本次摇号未中奖");
			messageList.add(messageDto);
			dingTalkMessageFacade.asyncSendBatchText(messageList);
		}
		
	}


}
