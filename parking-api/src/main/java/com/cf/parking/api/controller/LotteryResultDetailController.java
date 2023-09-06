package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryResultDetailPageReq;
import com.cf.parking.api.response.LotteryResultDetailPageRsp;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.cf.parking.facade.facade.LotteryResultDetailFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
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
    @PostMapping("/list")
    public Result<PageResponse<LotteryResultDetailPageRsp>> list(@RequestBody LotteryResultDetailPageReq param)
    {

        return null;
    }


    /**
     * 获取摇号结果详情详细信息
     */
    @PostMapping("/1")
    public Result getInfo(@RequestBody Long id)
    {
        return null;
    }

    /**
     * 新增摇号结果详情
     */
    @PostMapping("/2")
    public Result add(@RequestBody LotteryResultDetailPO lotteryResultDetailPO)
    {
        return null;
    }

    /**
     * 修改摇号结果详情
     */
    @PostMapping("/3")
    public Result edit(@RequestBody LotteryResultDetailPO lotteryResultDetailPO)
    {
        return null;
    }

    /**
     * 删除摇号结果详情
     */
	@PostMapping("/4")
    public Result remove(@RequestBody Long[] ids)
    {
        return null;
    }
}
