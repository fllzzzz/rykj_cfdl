package com.cf.parking.services.facade.impl;

import java.util.List;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryRuleAssignMapper;
import com.cf.parking.dao.po.LotteryRuleAssignPO;
import com.cf.parking.facade.bo.LotteryRuleAssignBO;
import com.cf.parking.facade.dto.LotteryRuleAssignDTO;
import com.cf.parking.facade.facade.LotteryRuleAssignFacade;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 摇号规则-停车场分配Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Service
public class LotteryRuleAssignFacadeImpl implements LotteryRuleAssignFacade
{
    @Autowired
    private LotteryRuleAssignMapper mapper;

    /**
     * 查询摇号规则-停车场分配列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<LotteryRuleAssignBO> getLotteryRuleAssignList(LotteryRuleAssignDTO dto) {
        Page<LotteryRuleAssignPO> page = PageUtils.toPage(dto);

        Page<LotteryRuleAssignPO> poPage = mapper.selectPage(page, new LambdaQueryWrapper<LotteryRuleAssignPO>()
                .eq(StringUtils.isNotBlank(dto.getType()), LotteryRuleAssignPO::getType, dto.getType())
                .eq(StringUtils.isNotBlank(dto.getName()), LotteryRuleAssignPO::getName, dto.getName())
                .eq(StringUtils.isNotBlank(dto.getParkingLotCode()), LotteryRuleAssignPO::getParkingLotCode, dto.getParkingLotCode())
                .eq(StringUtils.isNotBlank(dto.getState()), LotteryRuleAssignPO::getState, dto.getState())
                .orderByAsc(LotteryRuleAssignPO::getCreateTm));

        List<LotteryRuleAssignBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryRuleAssignBO.class);

        //TODO:根据停车场code查询停车场名称返回前端
        return PageUtils.toResponseList(page,boList);
    }


}
