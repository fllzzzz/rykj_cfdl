package com.cf.parking.api.controller;

import java.util.List;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;

import com.cf.parking.api.request.LotteryBatchPageReq;
import com.cf.parking.api.response.LotteryApplyRecordPageRsp;
import com.cf.parking.api.response.LotteryBatchPageRsp;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.facade.facade.LotteryBatchFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 摇号批次Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@RestController
@RequestMapping("/lottery/batch")
public class LotteryBatchController
{
    @Resource
    private LotteryBatchFacade lotteryBatchFacade;

    /**
     * 查询摇号批次列表
     */
    @PostMapping("/list")
    public Result<PageResponse<LotteryBatchPageRsp>> list(@RequestBody LotteryBatchPageReq param)
    {
        return null;
    }


    /**
     * 获取摇号批次详细信息
     */
    @PostMapping("/1")
    public Result getInfo(@RequestBody Long id)
    {
        return null;
    }

    /**
     * 新增摇号批次
     */
    @PostMapping("/2")
    public Result add(@RequestBody LotteryBatchPO lotteryBatchPO)
    {
        return null;
    }

    /**
     * 修改摇号批次
     */
    @PostMapping("/3")
    public Result edit(@RequestBody LotteryBatchPO lotteryBatchPO)
    {
        return null;
    }

    /**
     * 删除摇号批次
     */
	@PostMapping("/4")
    public Result remove(@RequestBody Long[] ids)
    {
        return null;
    }
}
