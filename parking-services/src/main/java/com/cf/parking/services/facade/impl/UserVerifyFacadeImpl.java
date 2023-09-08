package com.cf.parking.services.facade.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.UserVerifyMapper;
import com.cf.parking.dao.po.UserVerifyPO;
import com.cf.parking.facade.bo.UserVerifyBO;
import com.cf.parking.facade.dto.UserVerifyDTO;
import com.cf.parking.facade.dto.UserVerifyOptDTO;
import com.cf.parking.facade.facade.UserVerifyFacade;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import javax.annotation.Resource;
import java.util.Date;
import java.util.List;

/**
 * 车辆审核 Service业务层处理
 * @author
 * @date 2023/9/7
 */
@Slf4j
@Service
public class UserVerifyFacadeImpl implements UserVerifyFacade {

    @Resource
    private UserVerifyMapper mapper;

    /**
     * 查询车辆审核列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<UserVerifyBO> getUserVerifyList(UserVerifyDTO dto) {
        Page<UserVerifyPO> page = PageUtils.toPage(dto);

        //TODO:这里日期格式如何处理
        LambdaQueryWrapper<UserVerifyPO> queryWrapper = new LambdaQueryWrapper<UserVerifyPO>()
                .eq(StringUtils.isNoneBlank(dto.getState()), UserVerifyPO::getState, dto.getState())
                .like(StringUtils.isNoneBlank(dto.getUserName()), UserVerifyPO::getUserName, dto.getUserName())
                .le( !ObjectUtils.isEmpty(dto.getEndDate()) , UserVerifyPO::getCreateTm, dto.getEndDate())
                .ge(!ObjectUtils.isEmpty(dto.getStartDate()), UserVerifyPO::getCreateTm, dto.getStartDate())
                .orderByDesc(UserVerifyPO::getCreateTm);

        Page<UserVerifyPO> poPage = mapper.selectPage(page, queryWrapper);
        List<UserVerifyBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), UserVerifyBO.class);
        return PageUtils.toResponseList(page,boList);
    }

    /**
     * 获取摇车辆审核详细信息
     * @param dto
     * @return
     */
    @Override
    public UserVerifyBO getUserVerify(UserVerifyDTO dto) {
        UserVerifyPO userVerifyPO = mapper.selectById(dto.getId());

        UserVerifyBO bo = new UserVerifyBO();
        BeanUtils.copyProperties(userVerifyPO,bo);
        return bo;
    }

    /**
     * 新增车辆审核
     * @param dto
     * @return
     */
    @Override
    public Integer add(UserVerifyOptDTO dto) {
        UserVerifyPO userVerifyPO = new UserVerifyPO();
        BeanUtils.copyProperties(dto,userVerifyPO);
        userVerifyPO.setCreateTm(new Date());
        userVerifyPO.setUpdateTm(new Date());
        try{
            int result = mapper.insert(userVerifyPO);
            log.info("新增车辆审核成功  ——  {}",userVerifyPO);
            return result;
        }catch (Exception e){
            log.error("新增车辆审核失败：{}",e);
            return 0;
        }
    }

    /**
     * 审核车辆
     * @param dto
     * @return
     */
    @Override
    public Integer audit(UserVerifyOptDTO dto) {
        UserVerifyPO userVerifyPO = mapper.selectById(dto.getId());
        try{
            LambdaUpdateWrapper<UserVerifyPO> updateWrapper = new LambdaUpdateWrapper<UserVerifyPO>()
                    .eq(UserVerifyPO::getId, dto.getId())
                    .set(UserVerifyPO::getState, dto.getState())
                    .set(UserVerifyPO::getReason, dto.getReason());
            int result = mapper.update(userVerifyPO,updateWrapper);
            log.info("车辆审核成功，审核结果：{}，审核意见：{}，审核对象：{}", dto.getState(),dto.getReason(),userVerifyPO);
            return result;
        }catch (Exception e){
            log.error("车辆审核失败：审核对象：{}，报错原因{}",userVerifyPO,e);
            return 0;
        }
    }

    /**
     * 批量审核车辆
     * @param dto
     * @return
     */
    @Override
    public void batchAudit(UserVerifyOptDTO dto) {
        mapper.batchAudit(dto.getIds(),dto.getState(),dto.getReason());
    }

}
