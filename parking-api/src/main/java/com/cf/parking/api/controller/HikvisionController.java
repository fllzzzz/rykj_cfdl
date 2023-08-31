package com.cf.parking.api.controller;


import com.cf.parking.api.request.HikvisionReq;
import com.cf.parking.api.request.ParkingFreeReq;
import com.cf.parking.api.request.ParkingSpaceReq;
import com.cf.parking.api.response.ParkingFreeItemRsp;
import com.cf.parking.api.response.ParkingFreeRsp;
import com.cf.parking.api.response.ParkingSpaceRsp;
import com.cf.parking.facade.bo.ParkingFreeBO;
import com.cf.parking.facade.bo.ParkingSpaceBO;
import com.cf.parking.facade.dto.ParkingFreeDTO;
import com.cf.parking.facade.dto.ParkingSpaceDTO;
import com.cf.parking.services.service.HikvisionService;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 园区车位接口
 */
@Slf4j
//@AdminUserAuthentication
@RestController
@RequestMapping("/hikvision")
@Api(tags = "Hikvision模块")
public class HikvisionController {

    @Resource
    private HikvisionService hikvisionService;

    @ApiOperation(value = "海康安防通用获取接口", notes = "海康安防通用获取接口")
    @PostMapping("/getApiResult")
    public Result getApiResult(@RequestBody HikvisionReq param) {
        if (StringUtils.isBlank(param.getApiPath())) {
            return Result.buildErrorResult("path不能为空");
        }
        return hikvisionService.getApiResult(param.getApiPath(), param.getPageNo(), param.getPageSize());
    }

    @ApiOperation(value = "获取过车记录接口", notes = "获取过车记录接口")
    @PostMapping("/getCrossRecords")
    public Result getCrossRecords(@RequestBody HikvisionReq param) {
        param.setApiPath("/api/pms/v1/crossRecords/page");
        return hikvisionService.getCrossRecords(param.getApiPath(), param.getPageNo(), param.getPageSize());
    }

    @ApiOperation(value = "获取场内车停车信息接口", notes = "获取场内车停车信息接口")
    @PostMapping("/getTempCarInRecords")
    public Result<PageResponse<ParkingSpaceRsp>> getTempCarInRecords(@RequestBody ParkingSpaceReq param) {
        ParkingSpaceDTO dto = new ParkingSpaceDTO();
        BeanConvertorUtils.copy(param, dto);

        PageResponse<ParkingSpaceBO> result = hikvisionService.getTempCarInRecords(dto);
        List<ParkingSpaceRsp> parkingSpaceRsps = BeanConvertorUtils.copyList(result.getList(), ParkingSpaceRsp.class);
        return Result.buildSuccessResult(new PageResponse(parkingSpaceRsps, result.getPageNo(), result.getTotal(), result.getPageSize()));
    }

    @ApiOperation(value = "初始化车牌接口", notes = "初始化车牌接口")
    @RequestMapping("/initPlateNo")
    public Result initPlateNo() {
        return hikvisionService.initPlateNo();
    }

    @ApiOperation(value = "返回场内车位空置率", notes = "返回场内车位空置率")
    @RequestMapping("/getParkingFree")
    public Result<List<ParkingFreeRsp>> getParkingFree(@RequestBody ParkingFreeReq param) {
        ParkingFreeDTO dto = new ParkingFreeDTO();
        BeanConvertorUtils.copy(param, dto);
        List<ParkingFreeBO> list = hikvisionService.getList(dto);
        HashMap<String, List<ParkingFreeItemRsp>> map = new HashMap<>(list.size() / 3);
        ArrayList<String> order = new ArrayList<>();
        for (ParkingFreeBO parkingFreeBO : list) {
            String data = DateFormatUtils.format(parkingFreeBO.getStatDate(), "yyyy-MM-dd");

            List<ParkingFreeItemRsp> parkingFreeItemRsps = map.get(data);
            if (parkingFreeItemRsps == null) {
                order.add(data);
                parkingFreeItemRsps = new ArrayList<>();
            }
            ParkingFreeItemRsp parkingFreeItemRsp = new ParkingFreeItemRsp();
            parkingFreeItemRsp.setFreeNum(parkingFreeBO.getFreeNum());
            parkingFreeItemRsp.setParkFlag(parkingFreeBO.getParkFlag());
            parkingFreeItemRsps.add(parkingFreeItemRsp);
            map.put(data, parkingFreeItemRsps);
        }

        //  map是无序的，组装的时候乱序了，
        List<ParkingFreeRsp> collect = order.stream().map(key -> {
            ParkingFreeRsp parkingFreeRsp = new ParkingFreeRsp();
            parkingFreeRsp.setStatDate(key);
            parkingFreeRsp.setItemList(map.get(key));
            return parkingFreeRsp;
        }).collect(Collectors.toList());
        return Result.buildSuccessResult(collect);
    }
}
