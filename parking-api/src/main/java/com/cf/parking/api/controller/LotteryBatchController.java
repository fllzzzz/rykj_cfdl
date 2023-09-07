package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryBatchOptReq;
import com.cf.parking.api.request.LotteryBatchReq;
import com.cf.parking.api.response.LotteryBatchRsp;
import com.cf.parking.facade.bo.LotteryBatchBO;
import com.cf.parking.facade.dto.LotteryBatchDTO;
import com.cf.parking.facade.facade.LotteryBatchFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 摇号批次Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Api(tags = "摇号批次模块——摇号系统")
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
    @ApiOperation(value = "查询摇号批次列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<LotteryBatchRsp>> list(@RequestBody LotteryBatchReq param)
    {
        LotteryBatchDTO dto = new LotteryBatchDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<LotteryBatchBO> result = lotteryBatchFacade.getLotteryBatchList(dto);
        List<LotteryBatchRsp> lotteryBatchRsps = BeanConvertorUtils.copyList(result.getList(), LotteryBatchRsp.class);
        return Result.buildSuccessResult(new PageResponse(lotteryBatchRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
    }


    /**
     * 获取摇号批次详细信息
     */
    @ApiOperation(value = "获取摇号批次详细信息", notes = "点击修改，根据id查询")
    @PostMapping("/info")
    public Result<LotteryBatchRsp> getInfo(@RequestBody LotteryBatchReq param)
    {
        LotteryBatchDTO dto = new LotteryBatchDTO();
        BeanUtils.copyProperties(param,dto);

        LotteryBatchBO bo = lotteryBatchFacade.getInfo(dto);
        LotteryBatchRsp lotteryBatchRsp = new LotteryBatchRsp();
        BeanUtils.copyProperties(bo,lotteryBatchRsp);
        return Result.buildSuccessResult(lotteryBatchRsp);
    }

    /**
     * 新增摇号批次
     */
    @ApiOperation(value = "新增摇号批次", notes = "点击新增按钮")
    @PostMapping("/add")
    public Result add(@RequestBody LotteryBatchOptReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }

    /**
     * 修改摇号批次
     */
    @ApiOperation(value = "修改摇号批次", notes = "点击修改按钮")
    @PostMapping("/update")
    public Result edit(@RequestBody LotteryBatchOptReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }

    /**
     * 删除摇号批次
     */
    @ApiOperation(value = "删除摇号批次", notes = "点击删除按钮")
	@PostMapping("/delete")
    public Result remove(@RequestBody LotteryBatchReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }

    /**
     * 下发钉钉通知
     */
    @ApiOperation(value = "下发钉钉通知", notes = "点击通知按钮")
    @PostMapping("/notify")
    public Result notify(@RequestBody LotteryBatchReq param)
    {
        return Result.buildSuccessResult("接口暂未开发");
    }
}
