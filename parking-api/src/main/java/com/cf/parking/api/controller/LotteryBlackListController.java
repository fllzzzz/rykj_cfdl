package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryBlackListOptReq;
import com.cf.parking.api.request.LotteryBlackListReq;
import com.cf.parking.api.response.LotteryBlackListRsp;
import com.cf.parking.dao.po.LotteryBlackListPO;
import com.cf.parking.facade.facade.LotteryBlackListFacade;
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
 * 摇号黑名单Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Api(tags = "摇号黑名单模块——摇号系统")
@Slf4j
@RestController
@RequestMapping("/lottery/blackList")
public class LotteryBlackListController
{
    @Resource
    private LotteryBlackListFacade lotteryBlackListFacade;

    /**
     * 查询摇号黑名单列表
     */
    @ApiOperation(value = "查询摇号黑名单列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<LotteryBlackListRsp>> list(@RequestBody LotteryBlackListReq param)
    {
        return Result.buildSuccessResult();
    }


    /**
     * 获取摇号黑名单详细信息
     */
    @ApiOperation(value = "获取摇号黑名单详细信息", notes = "点击修改，根据id查询")
    @PostMapping("/info")
    public Result<LotteryBlackListRsp> getInfo(@RequestBody LotteryBlackListReq param)
    {
        return Result.buildSuccessResult();
    }

    /**
     * 新增摇号黑名单
     */
    @ApiOperation(value = "新增摇号黑名单", notes = "点击新增按钮")
    @PostMapping("/add")
    public Result add(@RequestBody LotteryBlackListOptReq param)
    {
        return Result.buildSuccessResult();
    }

    /**
     * 修改摇号黑名单
     */
    @ApiOperation(value = "修改摇号黑名单", notes = "点击修改按钮")
    @PostMapping("/update")
    public Result edit(@RequestBody LotteryBlackListOptReq param)
    {
        return Result.buildSuccessResult();
    }

    /**
     * 删除摇号黑名单
     */
    @ApiOperation(value = "移出摇号黑名单", notes = "点击移出按钮")
    @PostMapping("/move")
    public Result remove(@RequestBody LotteryBlackListReq param)
    {
        return Result.buildSuccessResult();
    }
}
