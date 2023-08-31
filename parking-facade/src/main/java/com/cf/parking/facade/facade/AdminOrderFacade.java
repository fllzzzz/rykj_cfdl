package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.AdminOrderRecordBO;
import com.cf.parking.facade.bo.AdminOrderRecordExportBO;
import com.cf.parking.facade.dto.AdminOrderRecordDTO;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;

import java.util.List;

/**
 * @author: lpy
 * @Date: 2022/10/21
 */
public interface AdminOrderFacade {
    /**
     * 查看订单分页
     *
     * @param adminOrderRecordDTO
     * @return
     */
    PageResponse<AdminOrderRecordBO> orderRecord(AdminOrderRecordDTO adminOrderRecordDTO);

    /**
     * 后台获取订单详情
     *
     * @param parkingOrderId
     * @return
     */
    Result orderDetail(Long parkingOrderId);

    /**
     * 后台订单导出
     *
     * @param adminOrderRecordDTO
     * @return
     */
    List<AdminOrderRecordExportBO> orderExport(AdminOrderRecordDTO adminOrderRecordDTO);
}
