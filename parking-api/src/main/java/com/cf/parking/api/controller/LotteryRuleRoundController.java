package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryRuleRoundPageReq;
import com.cf.parking.dao.po.LotteryRuleRoundPO;
import com.cf.parking.facade.facade.LotteryRuleRoundFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 摇号规则-轮数Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@RestController
@RequestMapping("/lottery/roundRule")
public class LotteryRuleRoundController
{
    @Resource
    private LotteryRuleRoundFacade lotteryRuleRoundFacade;

    /**
     * 查询摇号规则-轮数列表
     */
    @PostMapping("/list")
    public Result<PageResponse<LotteryRuleRoundPageReq>> list(@RequestBody LotteryRuleRoundPageReq param)
    {

        return null;
    }


    /**
     * 获取摇号规则-轮数详细信息
     */
    @PostMapping("/1")
    public Result getInfo(@RequestBody Long id)
    {
        return null;
    }

    /**
     * 新增摇号规则-轮数
     */
    @PostMapping("/2")
    public Result add(@RequestBody LotteryRuleRoundPO lotteryRuleRoundPO)
    {
        return null;
    }

    /**
     * 修改摇号规则-轮数
     */
    @PostMapping("/3")
    public Result edit(@RequestBody LotteryRuleRoundPO lotteryRuleRoundPO)
    {
        return null;
    }

    /**
     * 删除摇号规则-轮数
     */
	@PostMapping("/4")
    public Result remove(@RequestBody Long[] ids)
    {
        return null;
    }
}
