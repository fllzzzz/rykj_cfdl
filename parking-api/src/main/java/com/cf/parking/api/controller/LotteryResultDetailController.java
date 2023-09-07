package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryResultDetailReq;
import com.cf.parking.api.response.LotteryResultDetailPageRsp;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.cf.parking.facade.facade.LotteryResultDetailFacade;
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
 * 摇号结果详情Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Api(tags = "摇号结果详情模块——摇号系统")
@Slf4j
@RestController
@RequestMapping("/lottery/resultDetail")
public class LotteryResultDetailController
{
    @Resource
    private LotteryResultDetailFacade lotteryResultDetailFacade;

    /**
     * 查询摇号结果详情列表
     */
    @ApiOperation(value = "查询摇号结果详情列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<LotteryResultDetailPageRsp>> list(@RequestBody LotteryResultDetailReq param)
    {
        return Result.buildSuccessResult();
    }

}
