package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.OrderPeerPO;
import com.cf.parking.dao.po.ValidCountPO;
import org.apache.ibatis.annotations.Param;

import java.util.Date;
import java.util.List;

public interface OrderPeerPOMapper extends BaseMapper<OrderPeerPO> {
    int deleteByPrimaryKey(Long orderPeerId);


    int insertSelective(OrderPeerPO record);

    OrderPeerPO selectByPrimaryKey(Long orderPeerId);

    int updateByPrimaryKeySelective(OrderPeerPO record);

    int updateByPrimaryKey(OrderPeerPO record);

    /**
     * 查询时间范围内指定状态的单数
     *
     * @param beforeData 起始时间
     * @param afterData  结束时间
     * @param status     订单状态
     * @return 根据人分类的单数
     */
    List<ValidCountPO> getValidCount(@Param("beforeData") Date beforeData, @Param("afterData") Date afterData, @Param("status") Integer status);
}