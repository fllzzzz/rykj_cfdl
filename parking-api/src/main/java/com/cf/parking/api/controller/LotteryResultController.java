package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryResultReq;
import com.cf.parking.api.response.LotteryResultPageRsp;
import com.cf.parking.facade.facade.LotteryResultFacade;
import com.cf.support.authertication.AdminUserAuthentication;
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
 * 摇号结果Controller
 * 
 * @author
 * @date 2023-09-05
 */
@AdminUserAuthentication
@Api(tags = "摇号结果模块")
@Slf4j
@RestController
@RequestMapping("/lottery/result")
public class LotteryResultController
{
    @Resource
    private LotteryResultFacade lotteryResultFacade;

    /**
     * 查询摇号结果列表
     */
    @ApiOperation(value = "查询摇号结果列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<LotteryResultPageRsp>> list(@RequestBody LotteryResultReq param)
    {
        return Result.buildSuccessResult();
    }

    /**
     * 开始摇号
     */
    @ApiOperation(value = "开始摇号", notes = "点击开始摇号按钮")
    @PostMapping("/start")
    public Result start(@RequestBody LotteryResultReq param)
    {
    	lotteryResultFacade.lottery(param.getId());
        return Result.buildSuccessResult();
    }

    /**
     * 结果确认
     */
    @ApiOperation(value = "结果确认", notes = "点击结果确认按钮")
    @PostMapping("/confirm")
    public Result confirm(@RequestBody LotteryResultReq param)
    {
        return Result.buildSuccessResult();
    }

    /**
     * 结果发布
     */
    @ApiOperation(value = "结果发布", notes = "点击结果发布按钮")
	@PostMapping("/publish")
    public Result publish(@RequestBody LotteryResultReq param)
    {
        return Result.buildSuccessResult();
    }

    /**
     * 结果归档
     */
    @ApiOperation(value = "结果归档", notes = "点击结果归档按钮")
    @PostMapping("/archive")
    public Result archive(@RequestBody LotteryResultReq param)
    {
        return Result.buildSuccessResult();
    }
}
