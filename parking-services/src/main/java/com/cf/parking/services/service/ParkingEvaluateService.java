package com.cf.parking.services.service;

import cn.hutool.core.date.DateField;
import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.ParkingEvaluatePOMapper;
import com.cf.parking.dao.po.ParkingEvaluatePO;
import com.cf.parking.dao.po.ParkingOrderPO;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.parking.facade.enums.BooleanEnum;
import com.cf.parking.facade.enums.EvaluateEnum;
import com.cf.parking.facade.enums.EvaluateTypeEnum;
import com.cf.support.exception.BusinessException;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Service
public class ParkingEvaluateService extends ServiceImpl<ParkingEvaluatePOMapper, ParkingEvaluatePO> implements IService<ParkingEvaluatePO> {

    @Resource
    private ParkingEvaluatePOMapper parkingEvaluatePOMapper;

    /**
     * 根据orderId list 获取评论实体类list
     *
     * @param ids
     * @return
     */
    public List<ParkingEvaluatePO> queryEvaluateByOrderIds(List<Long> ids) {
        LambdaQueryWrapper<ParkingEvaluatePO> wrapper =
                new LambdaQueryWrapper<ParkingEvaluatePO>()
                        .in(ParkingEvaluatePO::getParkingOrderId, ids);
        return this.list(wrapper);
    }

    /**
     * 根据userId和订单号查询
     *
     * @param userId
     * @param parkingOrderId
     * @return
     */
    public ParkingEvaluatePO selectOneEvaluate(Long userId, Long parkingOrderId) {
        LambdaQueryWrapper<ParkingEvaluatePO> wrapper =
                new LambdaQueryWrapper<ParkingEvaluatePO>()
                        .eq(ParkingEvaluatePO::getUserId, userId)
                        .eq(ParkingEvaluatePO::getParkingOrderId, parkingOrderId);
        ParkingEvaluatePO parkingEvaluatePO = parkingEvaluatePOMapper.selectOne(wrapper);
        if (ObjectUtils.isEmpty(parkingEvaluatePO)) {
            throw new BusinessException(BizResultCodeEnum.EVALUATE_USER_ERROR.getMsg());
        }
        //是否已完成评价
        if (parkingEvaluatePO.getIsEvaluate().equals(EvaluateEnum.EVALUATED.getCode())) {
            throw new BusinessException(BizResultCodeEnum.EVALUATE_DONE.getMsg());
        }
        return parkingEvaluatePO;
    }

    @Transactional(rollbackFor = Exception.class)
    public void updateEvaluateById(ParkingEvaluatePO parkingEvaluatePO, Integer isEvaluate) {
        LambdaUpdateWrapper<ParkingEvaluatePO> wrapper =
                new LambdaUpdateWrapper<ParkingEvaluatePO>()
                        .eq(ParkingEvaluatePO::getParkingEvaluateId, parkingEvaluatePO.getParkingEvaluateId())
                        .eq(ParkingEvaluatePO::getIsEvaluate, parkingEvaluatePO.getIsEvaluate())
                        .set(ParkingEvaluatePO::getIsEvaluate, isEvaluate)
                        .set(ParkingEvaluatePO::getLevel, parkingEvaluatePO.getLevel())
                        .set(ParkingEvaluatePO::getEvaluateDesc, parkingEvaluatePO.getEvaluateDesc());
        if (!this.update(wrapper)) {
            throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH.getMsg());
        }
    }

    public Float selectAvg(Integer evaluateType, Long evaluateUserId) {
        ParkingEvaluatePO evaluatePO = new ParkingEvaluatePO()
                .setEvaluateType(evaluateType).setEvaluateUserId(evaluateUserId).setIsEvaluate(EvaluateEnum.EVALUATED.getCode());
        Float avg = parkingEvaluatePOMapper.selectAvg(evaluatePO);
        return avg != null ? avg : 0;
    }

    /**
     * 获取已完成评价记录
     *
     * @param evaluateType   评价类型
     * @param evaluateUserId 评价人id
     * @return
     */
    public List<ParkingEvaluatePO> getAllRecords(Integer evaluateType, Long evaluateUserId) {
        return this.list(new LambdaQueryWrapper<ParkingEvaluatePO>()
                .eq(ParkingEvaluatePO::getEvaluateUserId, evaluateUserId)
                .eq(ParkingEvaluatePO::getEvaluateType, evaluateType)
                .eq(ParkingEvaluatePO::getIsEvaluate, EvaluateEnum.EVALUATED.getCode()));
    }

    /**
     * 司机评价：按orderid、乘客userId（被评用户ID）查询评价表，看是否已评价，已评价直接返回
     *
     * @param userId
     * @param parkingOrderId
     * @return
     */
    public ParkingEvaluatePO queryEvaluateByOrderIdAndUserId(Long userId, Long parkingOrderId) {
        LambdaQueryWrapper<ParkingEvaluatePO> wrapper =
                new LambdaQueryWrapper<ParkingEvaluatePO>()
                        .eq(ParkingEvaluatePO::getEvaluateUserId, userId)
                        .eq(ParkingEvaluatePO::getParkingOrderId, parkingOrderId);
        return this.getOne(wrapper);
    }


    /**
     * 后台查看订单详情： 评价用户
     *
     * @param parkingOrderId
     * @return
     */
    public ParkingEvaluatePO userEvaluate(Long parkingOrderId, Integer state) {
        LambdaQueryWrapper<ParkingEvaluatePO> wrapper = new LambdaQueryWrapper<ParkingEvaluatePO>()
                .eq(ParkingEvaluatePO::getParkingOrderId, parkingOrderId)
                .eq(ParkingEvaluatePO::getEvaluateType, state);
        return this.getOne(wrapper);
    }


    /**
     * 自动更新24小时未评价的评价列表
     *
     * @return 修改是否成功
     */
    public boolean automaticOrder() {

        return this.update(new LambdaUpdateWrapper<ParkingEvaluatePO>()
                .eq(ParkingEvaluatePO::getEvaluateType, EvaluateTypeEnum.EVALUATE_DRIVER.getCode())
                .eq(ParkingEvaluatePO::getIsEvaluate, BooleanEnum.FALSE.getCode())
                .le(ParkingEvaluatePO::getCreateTm, DateUtil.offset(new Date(), DateField.HOUR, -24))
                .set(ParkingEvaluatePO::getIsEvaluate, BooleanEnum.TRUE.getCode())
                .set(ParkingEvaluatePO::getEvaluateAuto, BooleanEnum.TRUE.getCode())
                .set(ParkingEvaluatePO::getLevel, 5));
    }

    /**
     * 生成评价记录2条（乘客，司机）
     *
     * @param parkingOrderPO
     * @param userId
     */
    public void saveEvaluateRecord(ParkingOrderPO parkingOrderPO, Long userId) {
        Long driverId = parkingOrderPO.getUserId();
        Long parkingOrderId = parkingOrderPO.getParkingOrderId();
        List<ParkingEvaluatePO> evaluateList = new ArrayList<>();
        ParkingEvaluatePO passengerEvaluatePO = new ParkingEvaluatePO().setUserId(userId).setParkingOrderId(parkingOrderId)
                .setEvaluateType(EvaluateTypeEnum.EVALUATE_DRIVER.getCode()).setEvaluateUserId(driverId);
        evaluateList.add(passengerEvaluatePO);
        ParkingEvaluatePO driverEvaluatePO = new ParkingEvaluatePO().setUserId(driverId).setParkingOrderId(parkingOrderId)
                .setEvaluateType(EvaluateTypeEnum.EVALUATE_PASSENGER.getCode()).setEvaluateUserId(userId);
        evaluateList.add(driverEvaluatePO);
        this.saveBatch(evaluateList);
    }

    /**
     * 更新评价表
     *
     * @param parkingEvaluatePO 实体类
     * @return
     */
    public boolean updateEvaluateByOrderIdAndType(ParkingEvaluatePO parkingEvaluatePO) {
        LambdaUpdateWrapper<ParkingEvaluatePO> wrapper = new LambdaUpdateWrapper<ParkingEvaluatePO>()
                .eq(ParkingEvaluatePO::getParkingOrderId, parkingEvaluatePO.getParkingOrderId())
                .eq(ParkingEvaluatePO::getEvaluateType, EvaluateTypeEnum.EVALUATE_PASSENGER.getCode())
                .set(ParkingEvaluatePO::getUserId, parkingEvaluatePO.getUserId())
                .set(ParkingEvaluatePO::getEvaluateUserId, parkingEvaluatePO.getEvaluateUserId())
                .set(ParkingEvaluatePO::getIsEvaluate, EvaluateEnum.EVALUATED.getCode())
                .set(ParkingEvaluatePO::getEvaluateDesc, parkingEvaluatePO.getEvaluateDesc())
                .set(ParkingEvaluatePO::getLevel, parkingEvaluatePO.getLevel());
        return this.update(wrapper);
    }
}
