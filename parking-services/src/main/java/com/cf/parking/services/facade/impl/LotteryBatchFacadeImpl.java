package com.cf.parking.services.facade.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryBatchMapper;
import com.cf.parking.dao.po.*;
import com.cf.parking.facade.bo.LotteryBatchBO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.dto.LotteryBatchDTO;
import com.cf.parking.facade.dto.LotteryBatchOptDTO;
import com.cf.parking.facade.facade.LotteryBatchFacade;
import com.cf.parking.services.enums.LotteryBatchStateEnum;
import com.cf.parking.services.enums.LotteryResultStateEnum;
import com.cf.parking.services.service.LotteryBatchService;
import com.cf.parking.services.service.LotteryDealService;
import com.cf.parking.services.service.LotteryResultService;
import com.cf.parking.services.service.ParkingLotService;
import com.cf.parking.services.service.UserProfileService;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.DingTalkBean;
import com.cf.support.bean.IdWorker;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import javax.annotation.Resource;

/**
 * 摇号批次Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@Service
public class LotteryBatchFacadeImpl implements LotteryBatchFacade
{
    @Resource
    private LotteryBatchMapper mapper;

    @Resource
    private LotteryRuleRoundFacadeImpl lotteryRuleRoundFacade;

    @Resource
    private LotteryResultService lotteryResultService;

    @Resource
    private ParkingLotService parkingLotService;

    @Resource
    private UserProfileService userProfileService;

    @Resource
    private LotteryBatchService lotteryBatchService;

    @Resource
    private DingTalkBean dingTalkBean;
    
    @Resource
    private LotteryDealService lotteryDealService;

    @Resource
    private IdWorker idWorker;

    private final String  message = "%s期摇号报名时间：%s~%s，车位有效期：%s~%s，您可在报名有效期内报名摇号。";

    /**
     * 查询摇号批次列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<LotteryBatchBO> getLotteryBatchList(LotteryBatchDTO dto) {
        Page<LotteryBatchPO> page = PageUtils.toPage(dto);

        LambdaQueryWrapper<LotteryBatchPO> queryWrapper = new LambdaQueryWrapper<LotteryBatchPO>()
                .le(!ObjectUtils.isEmpty(dto.getEndDate()), LotteryBatchPO::getBatchNum, dto.getEndDate())
                .ge(!ObjectUtils.isEmpty(dto.getStartDate()) , LotteryBatchPO::getBatchNum,dto.getStartDate())
                .like(!ObjectUtils.isEmpty(dto.getRoundId()) , LotteryBatchPO::getRoundId, dto.getRoundId())
                .eq(StringUtils.isNotEmpty(dto.getState()), LotteryBatchPO::getState, dto.getState())
                .orderByDesc(LotteryBatchPO::getUpdateTm);

        Page<LotteryBatchPO> poPage = mapper.selectPage(page, queryWrapper);
        List<LotteryBatchBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryBatchBO.class);
        //生成摇号规则名称、摇号轮数数组
        boList.forEach(bo -> {
            String lotteryRule = lotteryRuleRoundFacade.getNameByRoundId(bo.getRoundId());
            bo.setLotteryRule(lotteryRule);
            Long[] roundIdArr = lotteryRuleRoundFacade.getRoundIdArrByRoundIdStr(bo.getRoundId());
            bo.setRoundIdArr(roundIdArr);
        });
        return PageUtils.toResponseList(page,boList);
    }


    /**
     * 新增摇号批次
     * @param dto
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer add(LotteryBatchOptDTO dto) {
        //1.参数复制
        LotteryBatchPO po = new LotteryBatchPO();
        BeanUtils.copyProperties(dto,po);

        //2.参数设置
        po.setId(idWorker.nextId());
        po.setState(LotteryBatchStateEnum.NEED_NOTIFY.getState());
        po.setCreateTm(new Date());
        po.setUpdateTm(new Date());
        //2.1轮数设置（将数组转为字符串）
        AssertUtil.checkNull(dto.getRoundIdArr(),"请选择摇号轮数");
        Long[] roundIdArr = dto.getRoundIdArr();
        String roundId = Arrays.toString(roundIdArr).replaceAll("\\s+","");
        po.setRoundId(roundId);

        try{
            //1.插入摇号批次记录
            int result = mapper.insert(po);
            log.info("新增摇号批次成功  ——  {}",po);

            //2.自动生成对应批次的摇号结果记录（选择了几轮就生成几条记录）
            List<LotteryResultPO> lotteryResultPOList = new ArrayList<>();

            for (Long round : roundIdArr) {
            	LotteryResultPO lotteryResultPO = new LotteryResultPO()
                        .setId(idWorker.nextId())
                        .setBatchId(po.getId())
                        .setBatchNum(po.getBatchNum())
                        .setRoundId(round)
                        .setState(LotteryResultStateEnum.UNLOTTERY.getState())
                        .setCreateTm(new Date())
                        .setUpdateTm(new Date());
                lotteryResultPOList.add(lotteryResultPO);
            }

            if (CollectionUtils.isNotEmpty(lotteryResultPOList)){
                lotteryResultService.saveBatch(lotteryResultPOList);
                lotteryResultPOList.clear();
            }

            return result;
        } catch (DataIntegrityViolationException e){
            log.error("新增摇号批次重复失败：{}，失败原因：{}",po,e);
            throw new BusinessException("该批次已存在");
        }
    }


    /**
     * 修改摇号批次
     * @param dto
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer update(LotteryBatchOptDTO dto) {
        LotteryBatchPO po = new LotteryBatchPO();
        BeanUtils.copyProperties(dto,po);
        po.setUpdateTm(new Date());
        //轮数设置（将数组转为字符串）
        AssertUtil.checkNull(dto.getRoundIdArr(),"请选择摇号轮数");
        Long[] roundIdArr = dto.getRoundIdArr();
        String roundId = Arrays.toString(roundIdArr).replaceAll("\\s+","");
        po.setRoundId(roundId);
            //1.修改前判断是否已通知
            LotteryBatchPO lotteryBatchPO = mapper.selectById(dto.getId());
            AssertUtil.checkNull(lotteryBatchPO, "批次记录不存在");
            AssertUtil.checkTrue(LotteryBatchStateEnum.NEED_NOTIFY.getState().equals(lotteryBatchPO.getState()), "状态不是待通知，不能进行修改");

            //1.修改摇号批次
            int result = mapper.updateById(po);
            log.info("修改摇号批次成功  ——  {}",po);

            //2.将之前的摇号结果删除
            lotteryResultService.batchDeleteByLotteryBatchId(po.getId());

            //3.生成新的摇号结果
            List<LotteryResultPO> lotteryResultPOList = new ArrayList<>();
            for (Long round : roundIdArr) {
            	LotteryResultPO lotteryResultPO = new LotteryResultPO()
                        .setId(idWorker.nextId())
                        .setBatchId(po.getId())
                        .setBatchNum(po.getBatchNum())
                        .setRoundId(round)
                        .setState(LotteryResultStateEnum.UNLOTTERY.getState())
                        .setCreateTm(new Date())
                        .setUpdateTm(new Date());
                lotteryResultPOList.add(lotteryResultPO);
            }
            if (CollectionUtils.isNotEmpty(lotteryResultPOList)){
                lotteryResultService.saveBatch(lotteryResultPOList);
                lotteryResultPOList.clear();
            }
            return result;
    }

    /**
     * 删除摇号批次
     * @param id
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer deleteById(Long id) {
            //1.删除前判断是否已通知
            LotteryBatchPO lotteryBatchPO = mapper.selectById(id);
            AssertUtil.checkNull(lotteryBatchPO, "批次记录不存在");
            AssertUtil.checkTrue(LotteryResultStateEnum.UNLOTTERY.getState().equals(lotteryBatchPO.getState()), "已通知，无法删除！");

            //2.删除摇号批次信息
            int result = mapper.deleteById(id);
            log.info("删除摇号批次成功，id：{}",id);

            //3.删除对应的摇号结果记录
            lotteryResultService.batchDeleteByLotteryBatchId(id);
            return result;
    }

    /**
     * 已结束的摇号批次进行结果查看
     * @param dto
     * @return
     */
    @Override
    public PageResponse<LotteryResultDetailBO> viewResult(LotteryBatchDTO dto) {
        Page<LotteryResultDetailPO> page = PageUtils.toPage(dto);
        PageResponse<LotteryResultDetailBO> boPageResponse = lotteryResultService.viewResult(page,dto.getId(),dto.getRoundId());
        return boPageResponse;
    }

    /**
     * 根据摇号轮数查询车位数量
     * @param roundIdArr
     * @return
     */
    @Override
    public Long getParkingAmountByRound(Long[] roundIdArr) {
        Long parkingAmount = 0L;
        for (Long roundId : roundIdArr) {
            //1.根据轮数查询停车场编码
            LotteryRuleRoundPO round = lotteryRuleRoundFacade.getLotteryRuleRoundByRoundId(roundId);
            if (round == null) {
            	continue;
            }
            //2.根据停车场编码查询车位数量
            ParkingLotPO parkingLot = parkingLotService.selectParkingLotByCode(round.getParkingLotCode());
            if (parkingLot == null) {
            	continue;
            }
            parkingAmount += parkingLot.getAmount();
        }
        return parkingAmount;
    }

    /**
     * 钉钉通知所有用户摇号批次信息
     * @param id
     */
    @Override
    public Integer notifyAllUserByBatchId(Long id) {
        LotteryBatchPO lotteryBatchPO = mapper.selectById(id);
        //1.钉钉通知
        //1.1查询所有用户
        List<UserProfilePO> userProfilePOS = userProfileService.queryBaseList();
        List<String> jobNumList = userProfilePOS.stream().filter(userProfilePO -> StringUtils.isNotBlank(userProfilePO.getJobNumber())).map(UserProfilePO::getJobNumber).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(jobNumList)){
            throw new BusinessException("未找到公司员工，无法通知！");
        }

        String notifyMessage = String.format(message, DateUtil.format(lotteryBatchPO.getBatchNum(), "yyyy-MM-dd"), DateUtil.format(lotteryBatchPO.getApplyStartTime(), "yyyy-MM-dd HH:mm:ss"),
                DateUtil.format(lotteryBatchPO.getApplyEndTime(), "yyyy-MM-dd HH:mm:ss"), DateUtil.format(lotteryBatchPO.getValidStartDate(), "yyyy-MM-dd"),
                DateUtil.format(lotteryBatchPO.getValidEndDate(), "yyyy-MM-dd"));

        dingTalkBean.sendTextMessage(notifyMessage,jobNumList);

        //2.修改批次状态为已通知
        lotteryBatchPO.setState(LotteryBatchStateEnum.HAVE_NOTIFIED.getState());
        return mapper.updateById(lotteryBatchPO);

    }

    /**
     * 判断本期车位有效期是否正确（本期车位有效开始日期要晚于上一批车位有效截止日期）
     * @param validStartDate
     * @return
     */
    @Override
    public boolean judgeValidStartDateUsable(Date validStartDate) {
        //1.查询上期（当前数据库内最新一期状态为非“待通知”的）
        LotteryBatchPO batchPO = lotteryBatchService.getNotifiedLatestBatchInfo();
        if (null != batchPO){
            return validStartDate.compareTo(batchPO.getValidEndDate()) >= 1;
        }
        return true;
    }


    @Transactional(rollbackFor = Exception.class)
	@Override
	public void allocationPark(Long id, String parkingCode) {
		LotteryBatchPO lotteryBatchPO = mapper.selectById(id);
		AssertUtil.checkNull(lotteryBatchPO, "批次数据不存在");
		AssertUtil.checkTrue(LotteryBatchStateEnum.HAVE_END.getState().equals(lotteryBatchPO.getState()), "当前状态不能进行分配");
		ParkingLotPO parking = parkingLotService.selectParkingLotByCode(parkingCode);
		AssertUtil.checkNull(parking, "停车场不存在");
		long num = mapper.updateByState(id,LotteryBatchStateEnum.HAVE_END.getState(),LotteryBatchStateEnum.ALLOCATIONED.getState());
		AssertUtil.checkTrue(num == 1, "状态已变更,请刷新重试");	
		lotteryDealService.allocationPark(lotteryBatchPO,parking);
    }


}
