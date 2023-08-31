package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.po.ParkingOrderPO;
import com.cf.parking.dao.po.QueryPO;
import com.cf.parking.dao.po.ValidCountPO;
import org.apache.ibatis.annotations.Param;

import java.util.Date;
import java.util.List;

/**
 * @author whx
 * @date 2022/10/19
 */

public interface ParkingOrderPOMapper extends BaseMapper<ParkingOrderPO> {

    int insert(ParkingOrderPO record);

    IPage<ParkingOrderPO> getOrderPage(Page<?> page, @Param("item") QueryPO queryPO);

    /**
     * 查询时间范围内有效单数
     *
     * @param beforeData 起始时间
     * @param afterData  结束时间
     * @param status     订单状态
     * @return 根据人分类的
     */
    List<ValidCountPO> getValidCount(@Param("beforeData") Date beforeData, @Param("afterData") Date afterData, @Param("status") Integer status);
}