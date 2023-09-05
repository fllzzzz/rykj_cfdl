package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryApplyRecordPageReq;
import com.cf.parking.api.response.LotteryApplyRecordPageRsp;
import com.cf.parking.facade.facade.LotteryApplyRecordFacade;
import com.cf.support.result.PageResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.cf.support.result.Result;

/**
 * 摇号申请记录Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@RestController
@RequestMapping("/lottery/applyRecord")
public class LotteryApplyRecordController
{
    @Resource
    private LotteryApplyRecordFacade lotteryApplyRecordFacade;

    /**
     * 查询摇号申请记录列表
     */
    @PostMapping("/page")
    public Result<PageResponse<LotteryApplyRecordPageRsp>>  getApplyRecordPage(@RequestBody LotteryApplyRecordPageReq param)
    {

//        PageResponse<> result = lotteryApplyRecordFacade.getApplyRecordPage();

//        return PageUtils.pageResult();
        return null;
    }


    /**
     * 获取摇号申请记录详细信息
     */
    @PostMapping("/4")
    public Result getInfo(@RequestBody Long id)
    {
        return null;
    }

    /**
     * 新增摇号申请记录
     */
    @PostMapping("/1")
    public Result add(@RequestBody LotteryApplyRecordPageReq lotteryApplyRecord)
    {
        return null;
    }

    /**
     * 修改摇号申请记录
     */
    @PostMapping("/2")
    public Result edit(@RequestBody LotteryApplyRecordPageReq lotteryApplyRecord)
    {
        return null;
    }

    /**
     * 删除摇号申请记录
     */
	@PostMapping("/3")
    public Result remove(@RequestBody LotteryApplyRecordPageReq ids)
    {
        return null;
    }
}
