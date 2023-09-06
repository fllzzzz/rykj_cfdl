package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryRuleAssignOptReq;
import com.cf.parking.api.request.LotteryRuleAssignReq;
import com.cf.parking.api.response.LotteryRuleAssignRsp;
import com.cf.parking.dao.po.LotteryRuleAssignPO;
import com.cf.parking.facade.facade.LotteryRuleAssignFacade;
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
 * 摇号规则-停车场分配Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Api(tags = "摇号规则-停车场分配模块")
@Slf4j
@RestController
@RequestMapping("/lottery/assignRule")
public class LotteryRuleAssignController
{
    @Resource
    private LotteryRuleAssignFacade lotteryRuleAssignFacade;

    /**
     * 查询摇号规则-停车场分配列表
     */
    @ApiOperation(value = "查询摇号规则-停车场分配列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<LotteryRuleAssignRsp>> list(@RequestBody LotteryRuleAssignReq param)
    {
        return null;
    }


    /**
     * 获取摇号规则-停车场分配详细信息
     */
    @ApiOperation(value = "停车场分配详细信息", notes = "点击修改，根据id查询")
    @PostMapping("/info")
    public Result<LotteryRuleAssignRsp> getInfo(@RequestBody LotteryRuleAssignReq param)
    {
        return null;
    }

    /**
     * 新增摇号规则-停车场分配
     */
    @ApiOperation(value = "新增摇号规则-停车场分配", notes = "点击新增按钮")
    @PostMapping("/add")
    public Result add(@RequestBody LotteryRuleAssignOptReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }

    /**
     * 修改摇号规则-停车场分配
     */
    @ApiOperation(value = "修改摇号规则-停车场分配", notes = "点击修改按钮")
    @PostMapping("/update")
    public Result edit(@RequestBody LotteryRuleAssignOptReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }

    /**
     * 删除摇号规则-停车场分配
     */
    @ApiOperation(value = "删除摇号规则-停车场分配", notes = "点击删除按钮")
    @PostMapping("/delete")
    public Result remove(@RequestBody LotteryRuleAssignReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }
}
