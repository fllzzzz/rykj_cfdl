package com.cf.parking.api.controller;

import java.util.List;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;

import com.cf.parking.api.request.LotteryBatchPageReq;
import com.cf.parking.api.request.LotteryBlackListPageReq;
import com.cf.parking.api.response.LotteryBatchPageRsp;
import com.cf.parking.api.response.LotteryBlackListPageRsp;
import com.cf.parking.dao.po.LotteryBlackListPO;
import com.cf.parking.facade.facade.LotteryBlackListFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import lombok.extern.java.Log;
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
 * 摇号黑名单Controller
 * 
 * @author
 * @date 2023-09-05
 */
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
    @PostMapping("/list")
    public Result<PageResponse<LotteryBlackListPageRsp>> list(@RequestBody LotteryBlackListPageReq param)
    {
        return null;
    }


    /**
     * 获取摇号黑名单详细信息
     */
    @PostMapping("/1")
    public Result getInfo(@RequestBody Long id)
    {
        return null;
    }

    /**
     * 新增摇号黑名单
     */
    @PostMapping("/2")
    public Result add(@RequestBody LotteryBlackListPO lotteryBlackListPO)
    {
        return null;
    }

    /**
     * 修改摇号黑名单
     */
    @PostMapping("/3")
    public Result edit(@RequestBody LotteryBlackListPO lotteryBlackListPO)
    {
        return null;
    }

    /**
     * 删除摇号黑名单
     */
	@PostMapping("/4")
    public Result remove(@RequestBody Long[] ids)
    {
        return null;
    }
}
