package com.cf.parking.services.facade.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

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
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.authertication.UserAuthenticationServer;
import com.cf.support.authertication.token.dto.UserSessionDTO;
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
    private UserAuthenticationServer userAuthenticationServer;

    private UserSessionDTO getUser() {
        return userAuthenticationServer.getCurrentUser();
    }

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
                .eq(LotteryApplyRecordPO::getUserId, getUser().getUserId())
                .le(ObjectUtils.isNotEmpty(dto.getEndDate()), LotteryApplyRecordPO::getBatchNum, dto.getEndDate())
                .ge(ObjectUtils.isNotEmpty(dto.getStartDate()), LotteryApplyRecordPO::getBatchNum, dto.getStartDate())
                .orderByDesc(LotteryApplyRecordPO::getUpdateTm);

        Page<LotteryApplyRecordPO> lotteryApplyRecordPOPage = mapper.selectPage(page, queryWrapper);
        List<LotteryApplyRecordPO> records = lotteryApplyRecordPOPage.getRecords();
        records.forEach(this::applyResultSet);

        List<LotteryApplyRecordBO> lotteryApplyRecordBOList = BeanConvertorUtils.copyList(records, LotteryApplyRecordBO.class);

        return PageUtils.toResponseList(page,lotteryApplyRecordBOList);
    }

    //摇号申请记录中的摇号结果设置
    private void applyResultSet(LotteryApplyRecordPO x) {
        if (x.getResult().equals(LotteryApplyRecordStateEnum.NOTOPEN.getState())){
            x.setResult(LotteryApplyRecordStateEnum.NOTOPEN.getRemark());
        }
        if (x.getResult().equals(LotteryApplyRecordStateEnum.NOTGET.getState())){
            x.setResult(LotteryApplyRecordStateEnum.NOTGET.getRemark());
        }
        if (x.getResult().equals(LotteryApplyRecordStateEnum.GET.getState())){
            //摇号结果模块设置的是停车场名称，这里直接返回此字段即可
            x.setResult(x.getParkingLotCode());
        }
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
        //2.判断当前时间是否处于报名时间内
        boolean InTime = lotteryBatchService.judgeWhetherInApplyTime(lotteryBatchPO.getApplyStartTime(),lotteryBatchPO.getApplyEndTime());
        applyBO.setTimeState(InTime);
        if (InTime){
            //报名时间内————>查看是否已申请
            LotteryApplyRecordPO recordPO = mapper.selectOne(new LambdaQueryWrapper<LotteryApplyRecordPO>()
                    .eq(LotteryApplyRecordPO::getBatchNum, lotteryBatchPO.getBatchNum())
                    .eq(LotteryApplyRecordPO::getUserId,userId)
                    .eq(LotteryApplyRecordPO::getApplyState, LotteryApplyRecordStateEnum.HAVE_APPLIED.getState()));

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
                        .eq(LotteryApplyRecordPO::getApplyState,LotteryApplyRecordStateEnum.HAVE_APPLIED.getState()));

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
                    applyBO.setResult("恭喜您摇中" + parkingLotPO.getRegion() + "停车场！");
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
        LotteryApplyRecordPO lotteryApplyRecordPO = mapper.selectOne(new LambdaQueryWrapper<LotteryApplyRecordPO>()
                .eq(LotteryApplyRecordPO::getUserId, userId)
                .eq(LotteryApplyRecordPO::getBatchId, batchId)
                .eq(LotteryApplyRecordPO::getApplyState,LotteryApplyRecordStateEnum.CANCEL.getState()));

        //如果之前有，修改状态
        if (null != lotteryApplyRecordPO){
            lotteryApplyRecordPO.setApplyState(LotteryApplyRecordStateEnum.HAVE_APPLIED.getState());
            int result = mapper.updateById(lotteryApplyRecordPO);
            log.info("用户申请摇号：{}",lotteryApplyRecordPO);
            return result;
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


}
