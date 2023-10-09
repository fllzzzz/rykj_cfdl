package com.cf.parking.services.facade.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.cf.parking.dao.mapper.LotteryRuleDescriptionMapper;
import com.cf.parking.dao.po.LotteryRuleDescriptionPO;
import com.cf.parking.facade.bo.LotteryRuleDescriptionBO;
import com.cf.parking.facade.dto.LotteryRuleDescriptionOptDTO;
import com.cf.parking.facade.facade.LotteryRuleDescriptionFacade;
import com.cf.support.bean.IdWorker;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import javax.annotation.Resource;
import java.util.Date;
import java.util.List;

/**
 * 摇号规则描述
 * @author
 * @date 2023/10/8
 */
@Slf4j
@Service
public class LotteryRuleDescriptionFacadeImpl implements LotteryRuleDescriptionFacade {

    @Resource
    private LotteryRuleDescriptionMapper mapper;

    @Resource
    private IdWorker idWorker;

    /**
     * 查询唯一的规则描述记录
     * @return
     */
    @Override
    public LotteryRuleDescriptionBO getDescription() {
        List<LotteryRuleDescriptionPO> poList = mapper.selectList(new LambdaQueryWrapper<LotteryRuleDescriptionPO>().orderByDesc(LotteryRuleDescriptionPO::getUpdateTm));
        if (CollectionUtils.isEmpty(poList)){
            return new LotteryRuleDescriptionBO();
        }
        return BeanConvertorUtils.map(poList.get(0),LotteryRuleDescriptionBO.class);
    }

    /**
     * 编辑摇号规则描述信息
     * 没有记录（id）的话新增，有的话修改
     * @param dto
     * @return
     */
    @Override
    public Integer edit(LotteryRuleDescriptionOptDTO dto) {
        if (ObjectUtils.isEmpty(dto.getId())){
            LotteryRuleDescriptionPO po = new LotteryRuleDescriptionPO()
                    .setId(idWorker.nextId())
                    .setDescription(dto.getDescription())
                    .setCreateTm(new Date())
                    .setUpdateTm(new Date());
            log.info("新增摇号规则描述信息:{}", JSON.toJSONString(po));
            return mapper.insert(po);
        }
        LotteryRuleDescriptionPO po = new LotteryRuleDescriptionPO()
                .setId(dto.getId())
                .setDescription(dto.getDescription())
                .setUpdateTm(new Date());
        log.info("修改摇号规则描述信息:{}", JSON.toJSONString(po));
        return mapper.updateById(po);
    }
}
