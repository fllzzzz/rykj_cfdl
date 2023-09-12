package com.cf.parking.services.facade.impl;

import java.util.List;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryApplyRecordMapper;
import com.cf.parking.dao.po.LotteryApplyRecordPO;
import com.cf.parking.dao.po.ParkingOrderPO;
import com.cf.parking.facade.bo.LotteryApplyRecordBO;
import com.cf.parking.facade.dto.LotteryApplyRecordDTO;
import com.cf.parking.facade.facade.LotteryApplyRecordFacade;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.authertication.UserAuthenticationServer;
import com.cf.support.authertication.token.dto.UserSessionDTO;
import com.cf.support.bean.IdWorker;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.formula.functions.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * 摇号申请记录Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Service
public class LotteryApplyRecordFacadeImpl implements LotteryApplyRecordFacade
{
    @Resource
    private LotteryApplyRecordMapper mapper;

    @Resource
    private IdWorker idWorker;

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
                .eq(StringUtils.isNotBlank(dto.getResult()), LotteryApplyRecordPO::getResult, dto.getResult())
                .eq(LotteryApplyRecordPO::getUserId, getUser().getUserId())
                .le(ObjectUtils.isNotEmpty(dto.getEndDate()), LotteryApplyRecordPO::getBatchNum, dto.getEndDate())
                .ge(ObjectUtils.isNotEmpty(dto.getStartDate()), LotteryApplyRecordPO::getBatchNum, dto.getStartDate())
                .orderByDesc(LotteryApplyRecordPO::getUpdateTm);

        Page<LotteryApplyRecordPO> lotteryApplyRecordPOPage = mapper.selectPage(page, queryWrapper);
        List<LotteryApplyRecordBO> lotteryApplyRecordBOList = BeanConvertorUtils.copyList(lotteryApplyRecordPOPage.getRecords(), LotteryApplyRecordBO.class);

        return PageUtils.toResponseList(page,lotteryApplyRecordBOList);
    }



    /**
     * 新增摇号申请记录
     * 
     * @param po 摇号申请记录
     * @return 结果
     */
    public int insertLotteryApplyRecord(LotteryApplyRecordPO po)
    {
        po.setId(idWorker.nextId());
        return mapper.insert(po);
    }


}
