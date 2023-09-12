package com.cf.parking.services.facade.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryRuleRoundMapper;
import com.cf.parking.dao.po.LotteryBlackListPO;
import com.cf.parking.dao.po.LotteryRuleRoundPO;
import com.cf.parking.facade.bo.LotteryRuleRoundBO;
import com.cf.parking.facade.bo.LotteryRuleRoundBaseBO;
import com.cf.parking.facade.dto.LotteryRuleRoundDTO;
import com.cf.parking.facade.dto.LotteryRuleRoundOptDTO;
import com.cf.parking.facade.facade.LotteryRuleRoundFacade;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * 摇号规则-轮数Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@Service
public class LotteryRuleRoundFacadeImpl implements LotteryRuleRoundFacade
{
    @Autowired
    private LotteryRuleRoundMapper mapper;

    @Resource
    private IdWorker idWorker;

    /** 根据摇号轮数id转换成对应的轮数名称并进行拼接*/
    public String getNameByRoundId(String roundId){
        String name = null;
        if (StringUtils.isBlank(roundId)){
            return name;
        }

        if (roundId.contains(",")){
            String[] splitIds = roundId.split(",");
            List<String> nameList = new ArrayList<>();
            for (String splitId : splitIds) {
                LotteryRuleRoundPO roundPO = mapper.selectById(Long.parseLong(splitId));
                if (StringUtils.isNotBlank(roundPO.getName())){
                    nameList.add(roundPO.getName());
                }
            }
            name = nameList.stream().collect(Collectors.joining(","));
        }else {
            LotteryRuleRoundPO roundPO = mapper.selectById(Long.parseLong(roundId));
            name = StringUtils.isNotBlank(roundPO.getName()) ?  roundPO.getName() :  null;
        }

        return name;
    }

    /**
     * 查询摇号规则-轮数名称列表
     * @return
     */
    @Override
    public List<LotteryRuleRoundBaseBO> selectBaseList() {
        List<LotteryRuleRoundPO> poList = mapper.selectBaseList();
        List<LotteryRuleRoundBaseBO> baseBOList = BeanConvertorUtils.copyList(poList, LotteryRuleRoundBaseBO.class);
        return baseBOList;
    }

    /**
     * 查询摇号规则-轮数列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<LotteryRuleRoundBO> getLotteryRuleRoundList(LotteryRuleRoundDTO dto) {
        Page<LotteryRuleRoundPO> page = PageUtils.toPage(dto);

        Page<LotteryRuleRoundPO> poPage = mapper.selectPage(page, new LambdaQueryWrapper<LotteryRuleRoundPO>()
                .eq(ObjectUtils.isNotEmpty(dto.getId()), LotteryRuleRoundPO::getId, dto.getId())
                .like(StringUtils.isNotBlank(dto.getName()), LotteryRuleRoundPO::getName, dto.getName())
                .eq(StringUtils.isNotBlank(dto.getState()), LotteryRuleRoundPO::getState, dto.getState()));

        List<LotteryRuleRoundBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryRuleRoundBO.class);
        return PageUtils.toResponseList(page,boList);
    }

    /**
     * 新增摇号规则-轮数
     * @param dto
     * @return
     */
    @Override
    public Integer add(LotteryRuleRoundOptDTO dto) {
        LotteryRuleRoundPO po = new LotteryRuleRoundPO();
        BeanUtils.copyProperties(dto,po);
        po.setId(idWorker.nextId());
        po.setCreateTm(new Date());
        po.setUpdateTm(new Date());
        try{
            int result = mapper.insert(po);
            log.info("添加摇号规则-轮数成功  ——  {}",po);
            return result;
        }catch (Exception e){
            log.error("添加摇号规则-轮数失败：{}，失败原因：{}",po,e);
            return 0;
        }
    }

    /**
     * 修改摇号规则-轮数
     * @param dto
     * @return
     */
    @Override
    public Integer update(LotteryRuleRoundOptDTO dto) {
        LotteryRuleRoundPO po = new LotteryRuleRoundPO();
        BeanUtils.copyProperties(dto,po);
        po.setUpdateTm(new Date());
        try{
            int result = mapper.updateById(po);
            log.info("修改摇号规则-轮数成功  ——  {}",po);
            return result;
        }catch (Exception e){
            log.error("修改摇号规则-轮数失败：{}，失败原因：{}",po,e);
            return 0;
        }
    }

    /**
     * 删除摇号规则-轮数
     * @param id
     * @return
     */
    @Override
    public Integer deleteById(Long id) {
        try{
            int result = mapper.deleteById(id);
            log.info("摇号规则-轮数删除成功，id：{}",id);
            return result;
        }catch (Exception e){
            log.error("摇号规则-轮数删除失败，id：{}，失败原因：{}",id,e);
            return 0;
        }
    }


}
