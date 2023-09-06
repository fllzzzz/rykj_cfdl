package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryRuleAssignPageReq;
import com.cf.parking.api.response.LotteryRuleAssignPageRsp;
import com.cf.parking.dao.po.LotteryRuleAssignPO;
import com.cf.parking.facade.facade.LotteryRuleAssignFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
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
    @PostMapping("/list")
    public Result<PageResponse<LotteryRuleAssignPageRsp>> list(@RequestBody LotteryRuleAssignPageReq param)
    {

        return null;
    }


    /**
     * 获取摇号规则-停车场分配详细信息
     */
    @PostMapping("/1")
    public Result getInfo(@RequestBody Long id)
    {
        return null;
    }

    /**
     * 新增摇号规则-停车场分配
     */
    @PostMapping("/2")
    public Result add(@RequestBody LotteryRuleAssignPO lotteryRuleAssignPO)
    {
        return null;
    }

    /**
     * 修改摇号规则-停车场分配
     */
    @PostMapping("/3")
    public Result edit(@RequestBody LotteryRuleAssignPO lotteryRuleAssignPO)
    {
        return null;
    }

    /**
     * 删除摇号规则-停车场分配
     */
	@PostMapping("/4")
    public Result remove(@RequestBody Long[] ids)
    {
        return null;
    }
}
