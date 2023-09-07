package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.ParkingLotOptReq;
import com.cf.parking.api.request.ParkingLotReq;
import com.cf.parking.api.response.ParkingLotRsp;
import com.cf.parking.facade.facade.ParkingLotFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 停车场Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Api(tags = "停车场管理模块——摇号系统")
@Slf4j
@RestController
@RequestMapping("/parking/lot")
public class ParkingLotController
{
    @Resource
    private ParkingLotFacade parkingLotFacade;

    /**
     * 查询停车场主列表
     */
    @ApiOperation(value = "查询停车场列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<ParkingLotRsp>> list(@RequestBody ParkingLotReq param)
    {
        return null;
    }


    /**
     * 获取停车场详细信息
     */
    @ApiOperation(value = "获取停车场详细信息", notes = "点击修改，根据id查询")
    @PostMapping("/info")
    public Result<ParkingLotRsp> getInfo(@RequestBody ParkingLotReq param)
    {
        return null;
    }

    /**
     * 新增停车场
     */
    @ApiOperation(value = "新增摇号规则-轮数", notes = "点击新增按钮")
    @PostMapping("/add")
    public Result add(@RequestBody ParkingLotOptReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }

    /**
     * 修改停车场
     */
    @ApiOperation(value = "修改停车场", notes = "点击修改按钮")
    @PostMapping("/update")
    public Result edit(@RequestBody ParkingLotOptReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }

    /**
     * 删除停车场
     */
    @ApiOperation(value = "删除停车场", notes = "点击删除按钮")
    @PostMapping("/delete")
    public Result remove(@RequestBody ParkingLotReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }
}
