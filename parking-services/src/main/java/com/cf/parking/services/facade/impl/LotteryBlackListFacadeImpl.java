package com.cf.parking.services.facade.impl;

import java.util.Date;
import java.util.List;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryBlackListMapper;
import com.cf.parking.dao.po.LotteryBlackListPO;
import com.cf.parking.dao.po.UserProfilePO;
import com.cf.parking.facade.bo.LotteryBlackListBO;
import com.cf.parking.facade.dto.LotteryBlackListDTO;
import com.cf.parking.facade.dto.LotteryBlackListOptDTO;
import com.cf.parking.facade.facade.LotteryBlackListFacade;
import com.cf.parking.services.service.UserProfileService;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;

import cn.hutool.core.util.ObjectUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import javax.annotation.Resource;

/**
 * 摇号黑名单Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@Service
public class LotteryBlackListFacadeImpl implements LotteryBlackListFacade
{
    @Resource
    private LotteryBlackListMapper mapper;

    @Resource
    private UserProfileService userProfileService;

    @Resource
    private IdWorker idWorker;

    /**
     * 查询摇号黑名单列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<LotteryBlackListBO> getLotteryBlackList(LotteryBlackListDTO dto) {
        Page<LotteryBlackListPO> page = PageUtils.toPage(dto);

        Page<LotteryBlackListPO> poPage = mapper.selectPage(page, new LambdaQueryWrapper<LotteryBlackListPO>()
                .like(StringUtils.isNoneBlank(dto.getName()), LotteryBlackListPO::getName, dto.getName())
                .like(StringUtils.isNoneBlank(dto.getJobNumber()), LotteryBlackListPO::getJobNumber, dto.getJobNumber())
                .eq(ObjectUtil.isNotNull(dto.getType()), LotteryBlackListPO::getType,dto.getType())
                .orderByDesc(LotteryBlackListPO::getCreateTm));

        List<LotteryBlackListBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryBlackListBO.class);
        return PageUtils.toResponseList(page,boList);
    }

    /**
     * 新增摇号黑名单
     * @param dto
     * @return
     */
    @Override
    public Integer add(LotteryBlackListOptDTO dto) {
        //1.查询userId
        UserProfilePO userProfilePO = userProfileService.selectUserProfileByNameAndJobNumber(null,dto.getCode());
       // AssertUtil.checkNull(userProfilePO, "该用户未登录过钉钉应用，获取不到用户信息，无法加入");
        //2.设置对象
        LotteryBlackListPO po = new LotteryBlackListPO();
        BeanUtils.copyProperties(dto,po);
        po.setId(idWorker.nextId());
        po.setJobNumber(dto.getCode());
        po.setUserId(userProfilePO == null ? 0l : userProfilePO.getUserId());
        po.setCreateTm(new Date());
        po.setUpdateTm(new Date());
        try{
            int result = mapper.insert(po);
            log.info("添加黑名单成功  ——  {}",po);
            return result;
        } catch (DataIntegrityViolationException e){
            log.error("添加重复：{}，失败原因：{}",po,e);
            throw new BusinessException("该用户已添加过，无须重复添加");
        } 
    }

    /**
     * 修改摇号黑名单
     * @param dto
     * @return
     */
    @Override
    public Integer update(LotteryBlackListOptDTO dto) {
        //2.设置对象
        LotteryBlackListPO po = new LotteryBlackListPO();
        BeanUtils.copyProperties(dto,po);
        po.setUpdateTm(new Date());
        
        int result = mapper.updateById(po);
        log.info("修改黑名单成功  ——  {}",po);
        return result;
        
    }

    /**
     * 移出摇号黑名单
     * @param id
     * @return
     */
    @Override
    public Integer deleteById(Long id) {
         int result = mapper.deleteById(id);
         log.info("移出摇号黑名单成功，id：{}",id);
         return result;
    }


}
