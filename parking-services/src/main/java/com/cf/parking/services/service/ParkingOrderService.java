package com.cf.parking.services.service;

import cn.hutool.core.date.DateField;
import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.ParkingOrderPOMapper;
import com.cf.parking.dao.po.ParkingOrderPO;
import com.cf.parking.dao.po.ValidCountPO;
import com.cf.parking.facade.dto.AdminOrderRecordDTO;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.parking.facade.enums.NoticedEnum;
import com.cf.parking.facade.enums.OrderStateEnum;
import com.cf.parking.services.utils.EmptyUtils;
import com.cf.support.exception.BusinessException;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * @author whx
 * @date 2022/10/19
 */
@Service
public class ParkingOrderService extends ServiceImpl<ParkingOrderPOMapper, ParkingOrderPO> implements IService<ParkingOrderPO> {
    @Resource
    public ParkingOrderPOMapper parkingOrderPOMapper;

    /**
     * 查询订单分页
     *
     * @param page
     * @param parkingOrderPO
     * @return
     */
    public IPage getOrderPage(Page page, ParkingOrderPO parkingOrderPO) {
        LambdaQueryWrapper<ParkingOrderPO> queryWrapper =
                new LambdaQueryWrapper<ParkingOrderPO>()
                        .eq(ParkingOrderPO::getOrderState, OrderStateEnum.ORDER_NOT_START.getCode())
                        .gt(ParkingOrderPO::getOrderTime, new Date())
                        //起始地、目的地模糊查询且不为空
                        .like(StringUtils.isNotBlank(parkingOrderPO.getStartAddress()), ParkingOrderPO::getStartAddress, parkingOrderPO.getStartAddress())
                        .like(StringUtils.isNotBlank(parkingOrderPO.getDestAddress()), ParkingOrderPO::getDestAddress, parkingOrderPO.getDestAddress())
                        .orderByAsc(ParkingOrderPO::getOrderTime);
        return parkingOrderPOMapper.selectPage(page, queryWrapper);
    }

    /**
     * 根据订单号列表查询
     *
     * @param ids
     * @return
     */
    public List<ParkingOrderPO> getOrderListByOrderIds(List<Long> ids) {
        return this.list(new LambdaQueryWrapper<ParkingOrderPO>().in(ParkingOrderPO::getParkingOrderId, ids));
    }

    /**
     * 更新订单
     *
     * @param po
     * @param orderState
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateOrder(ParkingOrderPO po, Integer orderState) {
        LambdaUpdateWrapper<ParkingOrderPO> updateWrapper =
                new LambdaUpdateWrapper<ParkingOrderPO>()
                        .eq(ParkingOrderPO::getParkingOrderId, po.getParkingOrderId())
                        .eq(ParkingOrderPO::getOrderState, po.getOrderState())
                        .set(ParkingOrderPO::getOrderState, orderState);
        if (!this.update(updateWrapper)) {
            throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH.getMsg());
        }
    }


    /**
     * 根据userId和orderTime查询订单列表
     *
     * @param parkingOrderPO
     * @return
     */
    public List<ParkingOrderPO> getOrderListByUserIdAndOrderTime(ParkingOrderPO parkingOrderPO) {
        LambdaQueryWrapper<ParkingOrderPO> queryWrapper = new LambdaQueryWrapper<ParkingOrderPO>()
                .eq(ParkingOrderPO::getUserId, parkingOrderPO.getUserId())
                .ne(ParkingOrderPO::getOrderState, OrderStateEnum.ORDER_CANCEL.getCode());
        return this.list(queryWrapper);
    }

    /**
     * 根据订单id获取订单
     *
     * @param parkingOrderId
     * @return
     */
    public ParkingOrderPO getOrderByOrderId(Long parkingOrderId) {
        return this.getById(parkingOrderId);
    }

    public List<ParkingOrderPO> getListByUserIdAndState(Long userId) {
        // 逻辑是用户id=id   状态等于未开始或进行中
        LambdaQueryWrapper<ParkingOrderPO> queryWrapper =
                new LambdaQueryWrapper<ParkingOrderPO>()
                        .eq(ParkingOrderPO::getUserId, userId)
                        .in(ParkingOrderPO::getOrderState, OrderStateEnum.ORDER_NOT_START.getCode(), OrderStateEnum.ORDER_ING.getCode())
                        .orderByAsc(ParkingOrderPO::getOrderTime);
        return this.list(queryWrapper);
    }

    /**
     * 通过orderId更新order 状态
     *
     * @param parkingOrderId, state
     * @return
     */
    public Boolean updateOrderStateByOrderId(Long parkingOrderId, Integer state) {
        LambdaUpdateWrapper<ParkingOrderPO> wrapper = new LambdaUpdateWrapper<ParkingOrderPO>()
                .eq(ParkingOrderPO::getParkingOrderId, parkingOrderId)
                .set(ParkingOrderPO::getOrderState, state);
        return this.update(wrapper);
    }

    /**
     * 根据userId获取orderList
     *
     * @param userId
     * @return
     */
    public IPage getOrderList(Page page, Long userId) {
        LambdaQueryWrapper<ParkingOrderPO> wrapper =
                new LambdaQueryWrapper<ParkingOrderPO>()
                        .eq(ParkingOrderPO::getUserId, userId)
                        .orderByDesc(ParkingOrderPO::getOrderTime);
        return parkingOrderPOMapper.selectPage(page, wrapper);
    }

    /**
     * 获取订单记录：通过orderId获取order List
     *
     * @param ids
     * @return
     */
    public IPage getOrderListByOrderIds(Page page, List<Long> ids) {
        LambdaQueryWrapper<ParkingOrderPO> wrapper = new LambdaQueryWrapper<ParkingOrderPO>()
                .in(ParkingOrderPO::getParkingOrderId, ids)
                .orderByDesc(ParkingOrderPO::getOrderTime);
        return parkingOrderPOMapper.selectPage(page, wrapper);
    }

    /**
     * 查询所有订单
     *
     * @return
     */
    public IPage queryAllOrder(Page page, AdminOrderRecordDTO param) {
        LambdaQueryWrapper<ParkingOrderPO> wrapper = new LambdaQueryWrapper<ParkingOrderPO>()
                .like(StringUtils.isNotEmpty(param.getDriverName()), ParkingOrderPO::getName, param.getDriverName())
                .eq(ObjectUtils.isNotEmpty(param.getParkingOrderId()), ParkingOrderPO::getParkingOrderId, param.getParkingOrderId())
                .eq(ObjectUtils.isNotEmpty(param.getOrderState()), ParkingOrderPO::getOrderState, param.getOrderState())
                .between(ObjectUtils.isNotEmpty(param.getOrderDate()), ParkingOrderPO::getOrderTime, DateUtil.beginOfDay(ObjectUtils.isEmpty(param.getOrderDate()) ? new Date() : param.getOrderDate()), DateUtil.endOfDay(ObjectUtils.isEmpty(param.getOrderDate()) ? new Date() : param.getOrderDate()))
                .orderByDesc(ParkingOrderPO::getCreateTm);
        return parkingOrderPOMapper.selectPage(page, wrapper);
    }


    /**
     * 后台导出查询
     *
     * @param param
     * @return
     */
    public List<ParkingOrderPO> queryExportOrder(AdminOrderRecordDTO param) {
        return this.list(new LambdaQueryWrapper<ParkingOrderPO>()
                .like(StringUtils.isNotEmpty(param.getDriverName()), ParkingOrderPO::getName, param.getDriverName())
                .eq(ObjectUtils.isNotEmpty(param.getParkingOrderId()), ParkingOrderPO::getParkingOrderId, param.getParkingOrderId())
                .eq(ObjectUtils.isNotEmpty(param.getOrderState()), ParkingOrderPO::getOrderState, param.getOrderState())
                .between(ObjectUtils.isNotEmpty(param.getOrderDate()), ParkingOrderPO::getOrderTime, DateUtil.beginOfDay(ObjectUtils.isEmpty(param.getOrderDate()) ? new Date() : param.getOrderDate()), DateUtil.endOfDay(ObjectUtils.isEmpty(param.getOrderDate()) ? new Date() : param.getOrderDate()))
                .orderByDesc(ParkingOrderPO::getCreateTm));
    }

    /**
     * 根据距离时间查询 即将开始和需要取消的订单
     *
     * @param minute 距离分钟
     * @return 订单列表
     */
    public List<ParkingOrderPO> getCancelAndStartOrder(int minute) {
        return list(new
                LambdaQueryWrapper<ParkingOrderPO>()
                .in(ParkingOrderPO::getOrderState, Arrays.asList(OrderStateEnum.ORDER_NOT_START.getCode()
                        , OrderStateEnum.ORDER_ING.getCode()))
                .ge(ParkingOrderPO::getOrderTime, new Date())
                .le(ParkingOrderPO::getOrderTime, DateUtil.offset(new Date(), DateField.MINUTE, minute))
                .eq(ParkingOrderPO::getNoticed, NoticedEnum.NOT_NOTICED.getCode()));

    }

    /**
     * 查询订单，为空校验
     *
     * @param userId
     * @param parkingOrderId
     * @return
     */
    public ParkingOrderPO getNotEmptyOrder(Long userId, Long parkingOrderId) {
        ParkingOrderPO parkingOrderPO = parkingOrderPOMapper.selectById(parkingOrderId);
        if (ObjectUtils.isEmpty(parkingOrderPO)) {
            EmptyUtils.emptyOrder(parkingOrderId, userId);
        }
        return parkingOrderPO;
    }

    /**
     * 查询时间范围内指定状态的单数
     *
     * @param beforeData 起始时间
     * @param afterData  结束时间
     * @param status     订单状态
     * @return 根据人分类的单数
     */
    public List<ValidCountPO> getValidCount(Date beforeData, Date afterData, Integer status) {
        return parkingOrderPOMapper.getValidCount(beforeData, afterData, status);
    }
}
