package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.OrderStateRecordPO;

public interface OrderStateRecordPOMapper extends BaseMapper<OrderStateRecordPO> {
    int deleteByPrimaryKey(Long orderStateRecordId);

    int insertSelective(OrderStateRecordPO record);

    OrderStateRecordPO selectByPrimaryKey(Long orderStateRecordId);

    int updateByPrimaryKeySelective(OrderStateRecordPO record);

    int updateByPrimaryKey(OrderStateRecordPO record);
}