package com.cf.parking.services.facade.impl;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;
import com.alibaba.fastjson.JSON;
import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryApplyRecordMapper;
import com.cf.parking.dao.po.*;
import com.cf.parking.facade.bo.LotteryApplyBO;
import com.cf.parking.facade.bo.LotteryApplyRecordBO;
import com.cf.parking.facade.dto.LotteryApplyRecordDTO;
import com.cf.parking.facade.facade.LotteryApplyRecordFacade;
import com.cf.parking.services.enums.LotteryApplyRecordStateEnum;
import com.cf.parking.services.service.*;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.stereotype.Service;
import javax.annotation.Resource;

/**
 * 摇号申请记录Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@Service
public class LotteryApplyRecordFacadeImpl implements LotteryApplyRecordFacade
{
    @Resource
    private LotteryApplyRecordMapper mapper;

    @Resource
    private IdWorker idWorker;

    @Resource
    private ParkingLotService parkingLotService;

    @Resource
    private LotteryBatchService lotteryBatchService;
    
    @Resource
    private LotteryResultService lotteryResultService;
    
    @Resource
    private LotteryResultDetailService lotteryResultDetailService;

    @Resource
    private UserProfileService userProfileService;
    
    @Resource
    private UserVerifyService userVerifyService;
    
    @Resource
    private LotteryRuleRoundService lotteryRuleRoundService;

    @Resource
    private ParkingInitService parkingInitService;


    /**
     * 查询摇号申请记录列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<LotteryApplyRecordBO> getApplyRecordList(LotteryApplyRecordDTO dto) {
        Page<LotteryApplyRecordPO> page = PageUtils.toPage(dto);

        LambdaQueryWrapper<LotteryApplyRecordPO> queryWrapper = new LambdaQueryWrapper<LotteryApplyRecordPO>()
                .eq(ObjectUtils.isNotEmpty(dto.getUserId()), LotteryApplyRecordPO::getUserId, dto.getUserId())
                .eq(ObjectUtils.isNotEmpty(dto.getResult()), LotteryApplyRecordPO::getResult, dto.getResult())
                .le(ObjectUtils.isNotEmpty(dto.getEndDate()), LotteryApplyRecordPO::getBatchNum, dto.getEndDate())
                .ge(ObjectUtils.isNotEmpty(dto.getStartDate()), LotteryApplyRecordPO::getBatchNum, dto.getStartDate())
                .orderByDesc(LotteryApplyRecordPO::getUpdateTm);

        Page<LotteryApplyRecordPO> lotteryApplyRecordPOPage = mapper.selectPage(page, queryWrapper);
        List<LotteryApplyRecordPO> records = lotteryApplyRecordPOPage.getRecords();
        records.forEach(this::applyResultSet);

        List<LotteryApplyRecordBO> lotteryApplyRecordBOList = BeanConvertorUtils.copyList(records, LotteryApplyRecordBO.class);

        return PageUtils.toResponseList(page,lotteryApplyRecordBOList);
    }


    /**
     * 个人申请摇号页面信息查询
     * 报名时间内————>查看是否已申请
     * 要求：若不在摇号申请规定的时间内，展示最近一次摇号报名申请及摇号结果和有效期
     * 报名时间外————>查询（最近一期的）的摇号结果（当前时间还未到达开始时间，则查询最近一次结果；
     *                                      如果超过结束时间，判断是否已发布/已结束）
     * @param userId
     * @return
     */
    @Override
    public LotteryApplyBO info(Long userId) {
        LotteryApplyBO applyBO = new LotteryApplyBO();

        //1.查询最新一期摇号批次信息（期号最大的、状态为已通知或已结束的）
        LotteryBatchPO lotteryBatchPO = lotteryBatchService.getNotifiedLatestBatchInfo();
        if (null == lotteryBatchPO){
            return new LotteryApplyBO();
        }
        applyBO.setBatchId(lotteryBatchPO.getId());
        applyBO.setApplyStartTime(lotteryBatchPO.getApplyStartTime());
        applyBO.setApplyEndTime(lotteryBatchPO.getApplyEndTime());
        applyBO.setValidStartDate(lotteryBatchPO.getValidStartDate());
        applyBO.setValidEndDate(lotteryBatchPO.getValidEndDate());
        applyBO.setBatchNum(lotteryBatchPO.getBatchNum());
        List<Long> roundList = JSON.parseArray(lotteryBatchPO.getRoundId(), Long.class);
        log.info("批次{}包含的轮次数据为：{}",lotteryBatchPO.getId() ,roundList);
        applyBO.setParkingLotName(getParkingLotName(roundList));
        //2.判断当前时间是否处于报名时间内
        boolean InTime = lotteryBatchService.judgeWhetherInApplyTime(lotteryBatchPO.getApplyStartTime(),lotteryBatchPO.getApplyEndTime());
        applyBO.setTimeState(InTime);
        if (InTime){
            //报名时间内————>查看是否已申请
            LotteryApplyRecordPO recordPO = mapper.selectOne(new LambdaQueryWrapper<LotteryApplyRecordPO>()
                    .eq(LotteryApplyRecordPO::getBatchNum, lotteryBatchPO.getBatchNum())
                    .eq(LotteryApplyRecordPO::getUserId,userId)
                    .eq(LotteryApplyRecordPO::getApplyState, LotteryApplyRecordStateEnum.HAVE_APPLIED.getState())
                    .last(" limit 1 ")
            		);

            applyBO.setApplyState(null != recordPO);
        }else {
            //报名时间外
            if ((new Date()).before(lotteryBatchPO.getApplyStartTime())){
                applyBO.setResult("当前未到报名时间！");
                applyBO.setResultColor(0);
            }else {
                LotteryApplyRecordPO applyRecordPO = mapper.selectOne(new LambdaQueryWrapper<LotteryApplyRecordPO>()
                        .eq(LotteryApplyRecordPO::getBatchId, lotteryBatchPO.getId())
                        .eq(LotteryApplyRecordPO::getUserId, userId)
                        .eq(LotteryApplyRecordPO::getApplyState,LotteryApplyRecordStateEnum.HAVE_APPLIED.getState())
                        .last(" limit 1 ")
                		);

                if (null == applyRecordPO){
                    applyBO.setResult("您本期未参加摇号报名！");
                    applyBO.setResultColor(1);
                    return applyBO;
                }


                //结果是否已发布
                List<LotteryResultPO> resultList = lotteryResultService.selectResultListByBatchId(lotteryBatchPO.getId());
                List<Long> resultIds = resultList.stream().filter(result -> Integer.parseInt(result.getState()) >= 4).map(LotteryResultPO::getId).collect(Collectors.toList());
                if (CollectionUtils.isEmpty(resultIds)){
                    applyBO.setResult("摇号结果暂未发布，请耐心等待！");
                    applyBO.setResultColor(2);
                    return applyBO;
                }
                LotteryResultDetailPO detailPO = lotteryResultDetailService.selectUserDetailByResultIds(userId,resultIds);
                if (null != detailPO){
                    ParkingLotPO parkingLotPO = parkingLotService.selectParkingLotByCode(detailPO.getParkingLotCode());
                    applyBO.setResult("恭喜您摇中" + parkingLotPO.getRegion() + "！");
                    applyBO.setResultColor(4);
                }else {
                    //如果都已经发布了
                    if (resultList.size() == resultIds.size() ){
                        applyBO.setResult("很遗憾您未摇中停车场！");
                        applyBO.setResultColor(3);
                    }else {
                        //如果有未发布的
                        applyBO.setResult("摇号结果暂未发布，请耐心等待！");
                        applyBO.setResultColor(2);
                    }
                }
            }
        }
        log.info("摇号信息展示:{}",JSON.toJSONString(applyBO));;
        return applyBO;
    }


	/**
     * 申请摇号
     * @param userId
     * @param batchId
     * @return
     */
    @Override
    public Integer apply(Long userId, Long batchId) {
    	List<UserVerifyPO>  verifyList = userVerifyService.queryVerifyListByUserIdList(Arrays.asList(userId));
    	AssertUtil.checkNull(verifyList, "您无审核过的车辆，不能报名摇号");
    	LotteryApplyRecordPO lotteryApplyRecordPO = mapper.selectOne(new LambdaQueryWrapper<LotteryApplyRecordPO>()
                .eq(LotteryApplyRecordPO::getUserId, userId)
                .eq(LotteryApplyRecordPO::getBatchId, batchId)
                .last(" limit 1 ")     
                );

        //如果之前有，修改状态
        if (null != lotteryApplyRecordPO){
            log.info("用户已申请摇号：{}",lotteryApplyRecordPO);
            return 1;
        }
        //之前没有的话新增
        //1.查询对应批次摇号信息
        LotteryBatchPO batchPO = lotteryBatchService.getById(batchId);
        UserProfilePO userProfile = userProfileService.getUserProfileByUserId(userId);

        LotteryApplyRecordPO insertApplyRecordPO = new LotteryApplyRecordPO();
        insertApplyRecordPO.setId(idWorker.nextId()).setBatchId(batchId).setBatchNum(batchPO.getBatchNum())
                .setParkingLotCode(null).setValidStartDate(batchPO.getValidStartDate())
                .setValidEndDate(batchPO.getValidEndDate()).setUserId(userId)
                .setUserName(userProfile.getName()).setJobNumber(userProfile.getJobNumber())
                .setApplyState(LotteryApplyRecordStateEnum.HAVE_APPLIED.getState())
                .setResult("-1").setCreateTm(new Date()).setUpdateTm(new Date());

        int result = mapper.insert(insertApplyRecordPO);
        log.info("用户申请摇号：{}",insertApplyRecordPO);
        return result;
    }

    /**
     * 取消摇号
     * @param userId
     * @param batchId
     * @return
     */
    @Override
    public Integer cancel(Long userId, Long batchId) {
        LotteryApplyRecordPO lotteryApplyRecordPO = mapper.selectOne(new LambdaQueryWrapper<LotteryApplyRecordPO>()
                .eq(LotteryApplyRecordPO::getUserId, userId)
                .eq(LotteryApplyRecordPO::getBatchId, batchId));

        if (null == lotteryApplyRecordPO){
            throw new BusinessException("未找到报名记录！");
        }
        return mapper.deleteById(lotteryApplyRecordPO.getId());
    }

  //摇号申请记录中的摇号结果设置
    private void applyResultSet(LotteryApplyRecordPO apply) {
        if (LotteryApplyRecordStateEnum.NOTOPEN.getState().equals(apply.getResult())){
        	apply.setResult(LotteryApplyRecordStateEnum.NOTOPEN.getRemark());
        }
        if (LotteryApplyRecordStateEnum.NOTGET.getState().equals(apply.getResult())){
        	apply.setResult(LotteryApplyRecordStateEnum.NOTGET.getRemark());
        }
        
        if (LotteryApplyRecordStateEnum.GET.getState().equals(apply.getResult())){
        	apply.setResult(apply.getParkingLotCode());
        }
        
    }



    /**
     * 判断当前时间是否在该摇号批次的报名时间内
     * @param batchId
     * @return
     */
    @Override
    public boolean judgeInApplyTime(Long batchId) {
        //1.查询该摇号批次信息
        LotteryBatchPO batchPO = lotteryBatchService.getById(batchId);
        if (null == batchPO){
            throw new BusinessException("未找到摇号报名批次信息");
        }

        //2.判断
        log.info("本期{}摇号报名时间：{}-{}，当前时间：{}",DateUtil.format(batchPO.getBatchNum(), "yyyy-MM-dd"), DateUtil.format(batchPO.getApplyStartTime(), "yyyy-MM-dd"),DateUtil.format(batchPO.getApplyEndTime(), "yyyy-MM-dd"),DateUtil.format(new Date(), "yyyy-MM-dd HH:mm:ss"));
        return lotteryBatchService.judgeWhetherInApplyTime(batchPO.getApplyStartTime(),batchPO.getApplyEndTime());
    }
    
    
    /**
     * 获取轮次对应的停车场
     * @param roundIdList
     * @return
     */
    private String getParkingLotName(List<Long> roundIdList) {
    	if (CollectionUtils.isEmpty(roundIdList)){
    		return null;
    	}
    	List<LotteryRuleRoundPO> roundList = lotteryRuleRoundService.listByIds(roundIdList);
    	log.info("根据id：{}查询轮次：{}",roundIdList,JSON.toJSONString(roundList));
    	if (CollectionUtils.isEmpty(roundList)){
    		return null;
    	}
    	List<String> parkingLot = roundList.stream().map(item -> item.getParkingLotCode()).collect(Collectors.toList());
        List<ParkingInitPO> initList =	parkingInitService.queryParkingInitList(parkingLot);
        log.info("根据车库编码：{}查询车库：{}",parkingLot,JSON.toJSONString(initList));
        String names = initList.stream().map(item -> item.getRegion()).collect(Collectors.joining(","));
        return names;
    }
}
