package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryResultPageReq;
import com.cf.parking.api.response.LotteryResultPageRsp;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.facade.facade.LotteryResultFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
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
    @PostMapping("/list")
    public Result<PageResponse<LotteryResultPageRsp>> list(@RequestBody LotteryResultPageReq param)
    {

        return null;
    }


    /**
     * 获取摇号结果详细信息
     */
    @PostMapping("/1")
    public Result getInfo(@RequestBody Long id)
    {
        return null;
    }

    /**
     * 新增摇号结果
     */
    @PostMapping("/2")
    public Result add(@RequestBody LotteryResultPO lotteryResultPO)
    {
        return null;
    }

    /**
     * 修改摇号结果
     */
    @PostMapping("/3")
    public Result edit(@RequestBody LotteryResultPO lotteryResultPO)
    {
        return null;
    }

    /**
     * 删除摇号结果
     */
	@PostMapping("/4")
    public Result remove(@RequestBody Long[] ids)
    {
        return null;
    }
}
