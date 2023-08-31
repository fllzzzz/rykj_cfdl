package com.cf.parking.services.facade.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.cf.parking.dao.po.CrossRecordsPO;
import com.cf.parking.facade.bo.CrossRecordsBO;
import com.cf.parking.facade.dto.CrossRecordsDTO;
import com.cf.parking.facade.dto.CrossRecordsQueryDTO;
import com.cf.parking.facade.dto.HikvisionResult;
import com.cf.parking.facade.enums.VehicleStateEnum;
import com.cf.parking.facade.facade.CrossRecordsFacade;
import com.cf.parking.services.integration.GatewayHikvisionFeign;
import com.cf.parking.services.service.CrossRecordsService;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author: lpy
 * @Date: 2023/03/28
 */
@Service
public class CrossRecordsFacadeImpl implements CrossRecordsFacade {
    @Resource
    private CrossRecordsService crossRecordsService;
    @Resource
    private GatewayHikvisionFeign gatewayHikvisionFeign;

    @Override
    public PageResponse<CrossRecordsBO> getPage(Long pageNo, Long pageSize, String plateNo) {
        IPage<CrossRecordsPO> iPage = crossRecordsService.getCrossRecordsByPlateNo(pageNo, pageSize, plateNo);
        if (iPage.getTotal() == 0) {
            return new PageResponse<>(new ArrayList<>(), iPage.getCurrent(), iPage.getTotal(), iPage.getSize());
        }
        List<CrossRecordsBO> userSpaceBOS = iPage.getRecords().stream().map(o -> {
            CrossRecordsBO crossRecordsBO = BeanConvertorUtils.map(o, CrossRecordsBO.class);
            crossRecordsBO.setCrossTime(DateUtil.format(o.getCrossTime(), "yyyy-MM-dd HH:mm:ss"));
            crossRecordsBO.setVehicleOut(this.getName(o.getVehicleOut()));
            return crossRecordsBO;
        }).collect(Collectors.toList());
        return PageUtils.toResponseList(iPage, userSpaceBOS);
    }

    public String getName(Integer code) {
        return VehicleStateEnum.IN.getCode().equals(code) ? VehicleStateEnum.IN.getMsg() : VehicleStateEnum.OUT.getMsg();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer saveCrossRecords(Integer pageNo) {
        // 每小时查询当天到当小时的过车记录
        String startTime = DateUtil.format(DateUtil.beginOfDay(new Date()), "yyyy-MM-dd HH:mm:ss").replace(" ", "T") + "+08:00";
        String endTime = DateUtil.format(DateUtil.beginOfHour(new Date()), "yyyy-MM-dd HH:mm:ss").replace(" ", "T") + "+08:00";
        CrossRecordsQueryDTO crossRecordsQueryDTO = new CrossRecordsQueryDTO().setPageNo(pageNo).setStartTime(startTime).setEndTime(endTime);

        HikvisionResult<PageResponse<CrossRecordsDTO>> crossRecords = gatewayHikvisionFeign.crossRecords("artemis/api/pms/v1/crossRecords/page", crossRecordsQueryDTO);
        List<CrossRecordsPO> crossRecordsPOS = new ArrayList<>();
        PageResponse<CrossRecordsDTO> pageResponse = crossRecords.getData();
        int total = Math.toIntExact(pageResponse.getTotal());
        if (total != 0) {
            List<CrossRecordsDTO> crossRecordsDTOS = pageResponse.getList();
            crossRecordsDTOS.forEach(crossRecord -> {
                CrossRecordsPO crossRecordsPO = BeanUtil.toBean(crossRecord, CrossRecordsPO.class);
                String crossTm = crossRecord.getCrossTime().replace("T", " ").split("\\+")[0];
                DateTime crossTime = DateUtil.parse(crossTm, "yyyy-MM-dd HH:mm:ss");
                crossRecordsPO.setParkingTitle(crossRecord.getParkName()).setCrossTime(crossTime);
                crossRecordsPOS.add(crossRecordsPO);
            });
            // 批量replace into 保存
            crossRecordsService.saveCrossRecords(crossRecordsPOS);
        }
        return total;
    }
}
