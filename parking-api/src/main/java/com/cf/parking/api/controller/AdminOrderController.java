package com.cf.parking.api.controller;

import com.cf.parking.api.request.AdminOrderRecordReq;
import com.cf.parking.api.request.OrderDetailReq;
import com.cf.parking.api.response.AdminOrderDetailRsp;
import com.cf.parking.api.response.AdminOrderRecordRsp;
import com.cf.parking.api.response.ExportAdminOrderRecordRsp;
import com.cf.parking.facade.bo.AdminOrderRecordBO;
import com.cf.parking.facade.bo.AdminOrderRecordExportBO;
import com.cf.parking.facade.dto.AdminOrderRecordDTO;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.parking.facade.facade.AdminOrderFacade;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.authertication.AdminUserAuthentication;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.ExcelUtiles;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;
import java.util.List;

/**
 * @author: lpy
 * @Date: 2022/10/21
 */
@RestController
@AdminUserAuthentication
@RequestMapping("/admin/order")
@Api(tags = "后台订单模块")
public class AdminOrderController {

    @Resource
    private AdminOrderFacade adminOrderFacade;


    /**
     * 订单记录分页查询
     *
     * @param param
     * @return
     */
    @ApiOperation(value = "订单记录分页查询", notes = "订单记录分页查询")
    @PostMapping(value = "/record")
    public Result<PageResponse<AdminOrderRecordRsp>> orderRecord(@RequestBody AdminOrderRecordReq param) {
        if (StringUtils.isNotEmpty(param.getDriverName()) && param.getDriverName().length() > 10) {
            return Result.buildResult(BizResultCodeEnum.PARAM_ERROR);
        }
        AdminOrderRecordDTO adminOrderRecordDTO = new AdminOrderRecordDTO();
        BeanConvertorUtils.copy(param, adminOrderRecordDTO);
        PageResponse<AdminOrderRecordBO> result = adminOrderFacade.orderRecord(adminOrderRecordDTO);
        if (result.getTotal() == 0) {
            return PageUtils.emptyPageResult(result);
        }
        List<AdminOrderRecordRsp> list = BeanConvertorUtils.copyList(result.getList(), AdminOrderRecordRsp.class);
        return PageUtils.pageResult(result, list);
    }


    /**
     * 后台查看订单详情
     *
     * @param param
     * @return
     */
    @ApiOperation(value = "查看订单详情", notes = "查看订单详情")
    @PostMapping(value = "/detail")
    public Result<AdminOrderDetailRsp> orderDetail(@RequestBody OrderDetailReq param) {
        if (ObjectUtils.isEmpty(param.getParkingOrderId())) {
            return Result.buildResult(BizResultCodeEnum.PARAM_ERROR);
        }

        Result result = adminOrderFacade.orderDetail(param.getParkingOrderId());
        if (result.isSuccess() && result.getData() != null) {
            return Result.buildSuccessResult(BeanConvertorUtils.map(result.getData(), AdminOrderDetailRsp.class));
        }
        return BeanConvertorUtils.map(result, Result.class);
    }


    @ApiOperation(value = "订单记录导出", notes = "订单记录导出")
    @PostMapping(value = "/export")
    public void orderExport(@RequestBody AdminOrderRecordReq param, HttpServletResponse response) {
        AdminOrderRecordDTO adminOrderRecordDTO = new AdminOrderRecordDTO();
        BeanConvertorUtils.copy(param, adminOrderRecordDTO);
        List<AdminOrderRecordExportBO> list = adminOrderFacade.orderExport(adminOrderRecordDTO);
        List<ExportAdminOrderRecordRsp> exportAdminOrderRecordRsps = BeanConvertorUtils.copyList(list, ExportAdminOrderRecordRsp.class);
        ExcelUtiles.exportExcel(exportAdminOrderRecordRsps, "订单记录", "订单记录", ExportAdminOrderRecordRsp.class, "订单记录.xls", response);
    }
}
