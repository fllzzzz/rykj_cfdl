package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.OrderStateRecordPOMapper;
import com.cf.parking.dao.po.OrderStateRecordPO;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author lpy
 * @date 2022/10/19
 */

@Service
public class OrderStateRecordService extends ServiceImpl<OrderStateRecordPOMapper, OrderStateRecordPO> implements IService<OrderStateRecordPO> {


    public Boolean saveOrderStateRecord(OrderStateRecordPO orderStateRecordPO) {
        return this.save(orderStateRecordPO);
    }

    public List<OrderStateRecordPO> queryOrderStateRecordByOrderId(Long parkingOrderId) {
        LambdaQueryWrapper<OrderStateRecordPO> wrapper = new LambdaQueryWrapper<OrderStateRecordPO>()
                .eq(OrderStateRecordPO::getParkingOrderId, parkingOrderId);
        return this.list(wrapper);
    }
}
