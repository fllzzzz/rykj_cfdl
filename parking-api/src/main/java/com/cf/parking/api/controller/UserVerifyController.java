package com.cf.parking.api.controller;

import com.cf.parking.api.request.LotteryRuleRoundOptReq;
import com.cf.parking.api.request.LotteryRuleRoundReq;
import com.cf.parking.api.request.UserVerifyOptReq;
import com.cf.parking.api.request.UserVerifyReq;
import com.cf.parking.api.response.LotteryRuleRoundRsp;
import com.cf.parking.api.response.UserVerifyRsp;
import com.cf.parking.facade.facade.UserVerifyFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

/**
 * @author
 * @date 2023/9/7
 */
@Api(tags = "车辆审核模块——摇号系统")
@Slf4j
@RestController
@RequestMapping("/user/verify")
public class UserVerifyController {

    @Resource
    private UserVerifyFacade userVerifyFacade;

    /**
     * 查询车辆审核列表
     */
    @ApiOperation(value = "查询车辆审核列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<UserVerifyRsp>> list(@RequestBody UserVerifyReq param)
    {
        return null;
    }


    /**
     * 获取摇车辆审核详细信息
     */
    @ApiOperation(value = "获取摇车辆审核详细信息", notes = "点击审核，根据id查询")
    @PostMapping("/info")
    public Result<UserVerifyRsp> getInfo(@RequestBody UserVerifyReq param)
    {
        return null;
    }

    /**
     * 新增车辆审核
     */
    @ApiOperation(value = "新增车辆审核", notes = "移动端个人中心模块点击车辆录入")
    @PostMapping("/add")
    public Result add(@RequestBody UserVerifyOptReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }

    /**
     * 审核车辆
     */
    @ApiOperation(value = "审核车辆", notes = "审核界面点击确定按钮")
    @PostMapping("/audit")
    public Result audit(@RequestBody UserVerifyOptReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }

    /**
     * 批量审核车辆
     */
    @ApiOperation(value = "批量审核车辆", notes = "点击批量审核按钮")
    @PostMapping("/batchAudit")
    public Result batchAudit(@RequestBody UserVerifyReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }
}
