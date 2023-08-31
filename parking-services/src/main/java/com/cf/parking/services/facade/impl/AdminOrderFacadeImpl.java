package com.cf.parking.services.facade.impl;

import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.cf.parking.dao.po.ParkingOrderPO;
import com.cf.parking.dao.po.OrderPeerPO;
import com.cf.parking.dao.po.OrderStateRecordPO;
import com.cf.parking.dao.po.ParkingEvaluatePO;
import com.cf.parking.facade.bo.AdminOrderDetailBO;
import com.cf.parking.facade.bo.AdminOrderRecordBO;
import com.cf.parking.facade.bo.AdminOrderRecordExportBO;
import com.cf.parking.facade.dto.AdminOrderRecordDTO;
import com.cf.parking.facade.enums.*;
import com.cf.parking.facade.facade.AdminOrderFacade;
import com.cf.parking.services.service.OrderPeerService;
import com.cf.parking.services.service.ParkingOrderService;
import com.cf.parking.services.service.OrderStateRecordService;
import com.cf.parking.services.service.ParkingEvaluateService;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author: lpy
 * @Date: 2022/10/21
 */
@Service
@Slf4j
public class AdminOrderFacadeImpl implements AdminOrderFacade {

    @Resource
    private ParkingOrderService parkingOrderService;
    @Resource
    private OrderPeerService orderPeerService;
    @Resource
    private OrderStateRecordService orderStateRecordService;
    @Resource
    private ParkingEvaluateService parkingEvaluateService;

    @Override
    public PageResponse<AdminOrderRecordBO> orderRecord(AdminOrderRecordDTO adminOrderRecordDTO) {
        // 查询所有订单
        IPage iPage = parkingOrderService.queryAllOrder(PageUtils.toPage(adminOrderRecordDTO), adminOrderRecordDTO);
        // 获取所有订单id, 过滤出未开始的订单,只是查同行记录表不要这个状态
        List<ParkingOrderPO> orders = iPage.getRecords();

        List<AdminOrderRecordBO> list = this.getAdminOrderRecordBOs(orders);
        return PageUtils.toResponseList(iPage, list);
    }

    /**
     * 后台查看订单详情
     *
     * @param
     * @return Result<AdminOrderDetailBO>
     */
    @Override
    public Result<AdminOrderDetailBO> orderDetail(Long parkingOrderId) {
        ParkingOrderPO order = parkingOrderService.getById(parkingOrderId);
        if (ObjectUtils.isEmpty(order)) {
            return Result.buildResult(BizResultCodeEnum.NO_ORDER);
        }
        AdminOrderDetailBO adminOrderDetailBO = BeanConvertorUtils.map(order, AdminOrderDetailBO.class);
        if (OrderStateEnum.ORDER_CANCEL.getCode().equals(order.getOrderState())) {
            // 取消操作。自动取消的判断
            List<OrderStateRecordPO> orderStateRecordPOS = orderStateRecordService.queryOrderStateRecordByOrderId(parkingOrderId);
            // 判断谁取消操作
            Integer cancelOperator = null;
            Date cancelTime = null;
            for (OrderStateRecordPO item : orderStateRecordPOS) {
                if (item.getOptType().equals(OptTypeEnum.DRIVER_CANCEL.getCode())) {
                    cancelOperator = UserTypeEnum.DRIVER.getCode();
                    cancelTime = item.getUpdateTm();
                } else if (item.getOptType().equals(OptTypeEnum.PASSENGER_CANCEL.getCode())) {
                    cancelOperator = UserTypeEnum.PASSENGER.getCode();
                    cancelTime = item.getUpdateTm();
                } else if (item.getOptType().equals(OptTypeEnum.CANCEL_ON_EXPIRE.getCode())) {
                    cancelOperator = UserTypeEnum.AUTO.getCode();
                    cancelTime = item.getUpdateTm();
                }
            }

            adminOrderDetailBO.setCancelUserType(cancelOperator);
            // 获取取消时间（这里取的是订单积分记录表的update_tm）
            adminOrderDetailBO.setCancelTime(cancelTime);
        }
        // 查询同行记录表
        List<OrderPeerPO> orderPeers = orderPeerService.getOrderPeerByStateAndOrderIds(Arrays.asList(parkingOrderId));
        List<String> collect = orderPeers.stream().map(item -> item.getJobNumber() + "/" + item.getName()).collect(Collectors.toList());
        String passengerInfo = ArrayUtils.toString(collect, ",").replace("[", "").replace("]", "");
        if (orderPeers.size() == 1) {
            OrderPeerPO peerPO = orderPeers.get(0);
            adminOrderDetailBO.setStartDistance(peerPO.getStartDistance()).setDestDistance(peerPO.getDestDistance())
                    .setGetInTm(peerPO.getGetInTm()).setGetOffTm(peerPO.getGetOffTm())
                    .setStartDestDistance(ObjectUtils.isEmpty(peerPO.getStartDestDistance()) ? new BigDecimal("0.00") : peerPO.getStartDestDistance());
        }
        // 查询评价表
        // 评价司机
        ParkingEvaluatePO driverEvaluate = parkingEvaluateService.userEvaluate(parkingOrderId, EvaluateTypeEnum.EVALUATE_DRIVER.getCode());
        // 评价乘客
        ParkingEvaluatePO passengerEvaluate = parkingEvaluateService.userEvaluate(parkingOrderId, EvaluateTypeEnum.EVALUATE_PASSENGER.getCode());

        String driverEvaluateDesc = ObjectUtils.isEmpty(driverEvaluate) ? "" : driverEvaluate.getEvaluateDesc();
        String passengerEvaluateDesc = ObjectUtils.isEmpty(passengerEvaluate) ? "" : passengerEvaluate.getEvaluateDesc();
        Integer driverLevel = ObjectUtils.isEmpty(driverEvaluate) ? 0 : driverEvaluate.getLevel();
        Integer passengerLevel = ObjectUtils.isEmpty(passengerEvaluate) ? 0 : passengerEvaluate.getLevel();

        String startAddress = order.getStartProvince() + order.getStartCity() + order.getStartCounty() + order.getStartAddress();
        String destAddress = order.getDestProvince() + order.getDestCity() + order.getStartCounty() + order.getDestAddress();
        adminOrderDetailBO.setPassengerInfoList(passengerInfo).setDriverJobNumber(order.getJobNumber()).setDriverName(order.getName())
                .setStartAddress(startAddress).setDestAddress(destAddress)
                .setDriverEvaluateDesc(driverEvaluateDesc).setDriverLevel(driverLevel)
                .setPassengerLevel(passengerLevel).setPassengerEvaluateDesc(passengerEvaluateDesc);

        return Result.buildSuccessResult(adminOrderDetailBO);
    }


    /**
     * 订单记录导出
     *
     * @param adminOrderRecordDTO
     * @return List<AdminOrderRecordBO>
     */
    @Override
    public List<AdminOrderRecordExportBO> orderExport(AdminOrderRecordDTO adminOrderRecordDTO) {
        // 查询所有订单
        List<ParkingOrderPO> orders = parkingOrderService.queryExportOrder(adminOrderRecordDTO);

        // 获取所有订单id, 过滤出未开始的订单,只是查同行记录表不要这个状态
        List<AdminOrderRecordBO> list = this.getAdminOrderRecordBOs(orders);

        if (CollectionUtils.isEmpty(list)) {
            return new ArrayList<>();
        }
        HashMap<Integer, String> map = new HashMap<>();
        map.put(OrderStateEnum.ORDER_NOT_START.getCode(), OrderStateEnum.ORDER_NOT_START.getMsg());
        map.put(OrderStateEnum.ORDER_ING.getCode(), OrderStateEnum.ORDER_ING.getMsg());
        map.put(OrderStateEnum.ORDER_CANCEL.getCode(), OrderStateEnum.ORDER_CANCEL.getMsg());
        map.put(OrderStateEnum.ORDER_COMPLETE.getCode(), OrderStateEnum.ORDER_COMPLETE.getMsg());
        map.put(OrderStateEnum.ORDER_OUT_TIME.getCode(), OrderStateEnum.ORDER_OUT_TIME.getMsg());
        return list.stream().map(item -> {
            AdminOrderRecordExportBO orderRecord = BeanConvertorUtils.map(item, AdminOrderRecordExportBO.class);
            String getInTm = ObjectUtils.isEmpty(item.getGetInTm()) ? "" : DateFormatUtils.format(item.getGetInTm(), "yyyy-MM-dd HH:mm");
            String getOffTm = ObjectUtils.isEmpty(item.getGetOffTm()) ? "" : DateFormatUtils.format(item.getGetOffTm(), "yyyy-MM-dd HH:mm");
            orderRecord.setOrderState(map.get(item.getOrderState())).setOrderTime(DateFormatUtils.format(item.getOrderTime(), "yyyy-MM-dd HH:mm"))
                    .setGetInTm(getInTm).setGetOffTm(getOffTm);
            return orderRecord;
        }).collect(Collectors.toList());
    }

    /**
     * 被导出订单方法和分页查看订单详情方法调用，
     *
     * @param orders
     * @return
     */
    public List<AdminOrderRecordBO> getAdminOrderRecordBOs(List<ParkingOrderPO> orders) {
        // 获取所有订单id, 过滤出未开始的订单,只是查同行记录表不要这个状态
        List<Long> ids = orders.stream().filter(item -> !OrderStateEnum.ORDER_NOT_START.getCode().equals(item.getOrderState()))
                .map(ParkingOrderPO::getParkingOrderId).collect(Collectors.toList());
        List<OrderPeerPO> orderPeers = new ArrayList<>(orders.size());
        if (!CollectionUtils.isEmpty(ids)) {
            orderPeers = orderPeerService.getOrderPeerByStateAndOrderIds(ids);
        }
        // 查询同行记录表
        Map<Long, OrderPeerPO> map = orderPeers.stream().distinct()
                .collect(Collectors.toMap(OrderPeerPO::getParkingOrderId, item -> item));

        // 把同行记录表中部分乘客数据组装到orders中
        return orders.stream()
                .map(item -> {
                    AdminOrderRecordBO adminOrderRecordBO = BeanConvertorUtils.map(item, AdminOrderRecordBO.class);
                    String startAddress = item.getStartProvince() + item.getStartCity() + item.getStartCounty() + item.getStartAddress();
                    String destAddress = item.getDestProvince() + item.getDestCity() + item.getDestCounty() + item.getDestAddress();

                    // 当用户列表为空的时候，直接展示空字符串
                    OrderPeerPO orderPeerPO = map.getOrDefault(item.getParkingOrderId(), new OrderPeerPO().setName("").setJobNumber(""));
                    String splitPattern = orderPeerPO == null ? "" : "/";
                    String passengerInfo = orderPeerPO.getJobNumber() + splitPattern + orderPeerPO.getName();
                    BigDecimal startDestDistance = ObjectUtils.isEmpty(orderPeerPO.getStartDestDistance()) ? new BigDecimal("0.00") : orderPeerPO.getStartDestDistance();
                    adminOrderRecordBO.setStartAddress(startAddress).setDestAddress(destAddress).setPassengerInfoList(passengerInfo)
                            .setDriverName(item.getName()).setDriverJobNumber(item.getJobNumber())
                            .setGetInTm(orderPeerPO.getGetInTm()).setGetOffTm(orderPeerPO.getGetOffTm())
                            .setStartDistance(orderPeerPO.getStartDistance()).setDestDistance(orderPeerPO.getDestDistance())
                            .setStartDestDistance(startDestDistance);
                    return adminOrderRecordBO;
                }).collect(Collectors.toList());
    }
}
