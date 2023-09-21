package com.cf.parking.services.facade.impl;

import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.UserVerifyMapper;
import com.cf.parking.dao.po.UserProfilePO;
import com.cf.parking.dao.po.UserVerifyPO;
import com.cf.parking.facade.bo.UserVerifyBO;
import com.cf.parking.facade.dto.UserVerifyDTO;
import com.cf.parking.facade.dto.UserVerifyOptDTO;
import com.cf.parking.facade.facade.UserVerifyFacade;
import com.cf.parking.services.constant.ParkingConstants;
import com.cf.parking.services.enums.UserVerifyStateEnum;
import com.cf.parking.services.service.UserProfileService;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import javax.annotation.Resource;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

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

    @Resource
    private IdWorker idWorker;

    @Resource
    private UserProfileService userProfileService;

    /**
     * 查询车辆审核列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<UserVerifyBO> getUserVerifyList(UserVerifyDTO dto) {
        Page<UserVerifyPO> page = PageUtils.toPage(dto);

        LambdaQueryWrapper<UserVerifyPO> queryWrapper = new LambdaQueryWrapper<UserVerifyPO>()
                .eq(!ObjectUtils.isEmpty(dto.getState()), UserVerifyPO::getState, dto.getState())
                .like(StringUtils.isNotBlank(dto.getUserName()), UserVerifyPO::getUserName, dto.getUserName())
                .le( !ObjectUtils.isEmpty(dto.getEndDate()) , UserVerifyPO::getCreateTm, DateUtil.format(dto.getEndDate(), "yyyy-MM-dd 23:59:59") )
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
        UserVerifyPO userVerifyPO = mapper.selectOne(new LambdaQueryWrapper<UserVerifyPO>()
                                                        .eq(UserVerifyPO::getUserId,dto.getUserId())
                                                        .eq(UserVerifyPO::getPlateNo,dto.getPlatNo()));

        return BeanConvertorUtils.map(userVerifyPO, UserVerifyBO.class);
    }

    /**
     * 新增车辆审核
     * @param dto
     * @return
     */
    @Override
    public Integer add(UserVerifyOptDTO dto) {
        //1.新增时车牌唯一性判断
        List<UserVerifyPO> poList = mapper.selectList(new LambdaQueryWrapper<UserVerifyPO>()
                .eq(UserVerifyPO::getPlateNo, dto.getPlateNo())
                .eq(UserVerifyPO::getUserId, dto.getUserId()));
        if (CollectionUtils.isNotEmpty(poList)){
            throw new BusinessException("车牌号已存在！");
        }

        //2.新增
        UserVerifyPO userVerifyPO = new UserVerifyPO();
        BeanUtils.copyProperties(dto,userVerifyPO);
        userVerifyPO.setId(idWorker.nextId());
        userVerifyPO.setState(Integer.parseInt(UserVerifyStateEnum.UNAUDIT.getState()));
        userVerifyPO.setCreateTm(new Date());
        userVerifyPO.setUpdateTm(new Date());
        try{
            int result = mapper.insert(userVerifyPO);
            log.info("新增车辆审核成功  ——  {}",userVerifyPO);
            return result;
        }catch (Exception e){
            log.error("新增车辆审核失败——  {}，失败原因：{}",userVerifyPO,e);
            return 0;
        }
    }

    /**
     * 审核车辆
     * @param dto
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer audit(UserVerifyOptDTO dto) {
        UserVerifyPO userVerifyPO = mapper.selectById(dto.getId());
        try{
            //1.审核
            LambdaUpdateWrapper<UserVerifyPO> updateWrapper = new LambdaUpdateWrapper<UserVerifyPO>()
                    .eq(UserVerifyPO::getId, dto.getId())
                    .set(UserVerifyPO::getState, dto.getState())
                    .set(UserVerifyPO::getReason, dto.getReason());
            int result = mapper.update(userVerifyPO,updateWrapper);
            log.info("车辆审核成功，审核结果：{}，审核意见：{}，审核对象：{}", dto.getState(),dto.getReason(),userVerifyPO);
            //2.修改用户默认停车场为"装配楼2期5F停车场"
            userProfileService.setDefaultParkingLotByUserId(userVerifyPO.getUserId());

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
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void batchAudit(UserVerifyOptDTO dto) {
        //1.批量审核
        Integer result = mapper.batchAudit(dto.getIds(), dto.getState(), dto.getReason());
        //2.如果是审核通过，为停车场信息为空的用户默认停车场
        //2.1查询用户ids
        List<UserVerifyPO> poList = mapper.selectBatchIds(dto.getIds());
        List<Long> userIds = poList.stream().filter(po->!ObjectUtils.isEmpty(po.getUserId())).map(UserVerifyPO::getUserId).collect(Collectors.toList());
        //2.2批量更新
        if (CollectionUtils.isNotEmpty(userIds) && UserVerifyStateEnum.SUCCESS.getState().equals(dto.getState().toString())){
            //为停车场信息为空的用户默认停车场
            List<UserProfilePO> userProfilePOList = userProfileService.selectList(userIds);
            List<Long> changeUserIds = userProfilePOList.stream().filter(po -> StringUtils.isBlank(po.getParkingLotRegion())).map(UserProfilePO::getUserId).collect(Collectors.toList());
            userProfileService.batchSetDefaultParkingLotByUserIds(changeUserIds,ParkingConstants.DEFAULT_PARKINGLOT);
        }
    }

    /**
     * 根据userId获取个人车牌号列表
     * @param userId
     * @return
     */
    @Override
    public List<String> getPlatNoListByUserId(Long userId) {
        return mapper.selectList(new LambdaQueryWrapper<UserVerifyPO>().eq(UserVerifyPO::getUserId, userId)).stream().map(UserVerifyPO::getPlateNo).collect(Collectors.toList());
    }

    /**
     * 修改审核车辆
     * @param dto
     * @return
     */
    @Override
    public Integer update(UserVerifyOptDTO dto) {
        UserVerifyPO userVerifyPO = mapper.selectById(dto.getId());

        //1.修改时判断车牌唯一性
        List<UserVerifyPO> poList = mapper.selectList(new LambdaQueryWrapper<UserVerifyPO>()
                .eq(UserVerifyPO::getPlateNo, dto.getPlateNo())
                .eq(UserVerifyPO::getUserId, dto.getUserId()));
        if (CollectionUtils.isNotEmpty(poList)){
            if (!poList.get(0).getId().equals(dto.getId())){
                throw new BusinessException("车牌号已存在！");
            }
        }

        //2.修改
        BeanUtils.copyProperties(dto,userVerifyPO);
        userVerifyPO.setState(Integer.parseInt(UserVerifyStateEnum.UNAUDIT.getState()));
        userVerifyPO.setReason("");
        userVerifyPO.setUpdateTm(new Date());
        try{
            int result = mapper.updateById(userVerifyPO);
            log.info("修改个人车辆审核信息成功  ——  {}",userVerifyPO);
            return result;
        }catch (Exception e){
            log.error("修改个人车辆审核信息失败：{}  ——  {}",e,userVerifyPO);
            return 0;
        }
    }

    /**
     * 删除个人车辆信息
     * @param id
     * @return
     */
    @Override
    public Integer deleteById(Long id) {
        return mapper.deleteById(id);
    }

}
