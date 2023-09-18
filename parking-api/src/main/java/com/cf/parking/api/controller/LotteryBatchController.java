package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryBatchOptReq;
import com.cf.parking.api.request.LotteryBatchReq;
import com.cf.parking.api.response.LotteryBatchRsp;
import com.cf.parking.api.response.LotteryResultDetailPageRsp;
import com.cf.parking.facade.bo.LotteryBatchBO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.dto.LotteryBatchDTO;
import com.cf.parking.facade.dto.LotteryBatchOptDTO;
import com.cf.parking.facade.facade.LotteryBatchFacade;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.models.auth.In;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
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
     * 根据摇号轮数查询车位数量
     */
    @ApiOperation(value = "根据摇号轮数查询车位数量", notes = "根据摇号轮数查询车位数量")
    @PostMapping("/parkingAmountByRound")
    public Result<Long> parkingAmountByRound(@RequestBody LotteryBatchOptReq param)
    {
        AssertUtil.checkNull(param.getRoundIdArr(),"请选择摇号轮数！");

        Long parkingAmount = lotteryBatchFacade.getParkingAmountByRound(param.getRoundIdArr());
        return Result.buildSuccessResult(parkingAmount);
    }

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
        Result<PageResponse<LotteryBatchRsp>> resultRsp = Result.buildSuccessResult(new PageResponse(lotteryBatchRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
        return resultRsp;
    }

    /**
     * 新增摇号批次
     */
    @ApiOperation(value = "新增摇号批次", notes = "点击新增按钮")
    @PostMapping("/add")
    public Result add(@RequestBody LotteryBatchOptReq param)
    {
        LotteryBatchOptDTO dto = new LotteryBatchOptDTO();
        BeanUtils.copyProperties(param,dto);

        Integer result = lotteryBatchFacade.add(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("批次新增失败，请重试！");
    }

    /**
     * 修改摇号批次
     */
    @ApiOperation(value = "修改摇号批次", notes = "点击修改按钮")
    @PostMapping("/update")
    public Result edit(@RequestBody LotteryBatchOptReq param)
    {
        LotteryBatchOptDTO dto = new LotteryBatchOptDTO();
        BeanUtils.copyProperties(param,dto);

        Integer result = lotteryBatchFacade.update(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("批次修改失败，请重试！");
    }

    /**
     * 删除摇号批次
     */
    @ApiOperation(value = "删除摇号批次", notes = "点击删除按钮")
	@PostMapping("/delete")
    public Result delete(@RequestBody LotteryBatchReq param)
    {
        AssertUtil.checkNull(param.getId(),"请选择要删除的批次！");

        Integer result = lotteryBatchFacade.deleteById(param.getId());
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("批次删除失败，请重试！");
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


    /**
     * 结果查看
     */
    @ApiOperation(value = "结果查看", notes = "点击结果查看按钮")
    @PostMapping("/viewResult")
    public Result<PageResponse<LotteryResultDetailPageRsp>> viewResult(@RequestBody LotteryBatchReq param)
    {
        AssertUtil.checkNull(param.getId(),"请选择摇号批次！");
        AssertUtil.checkNull(param.getRoundId(),"请选择摇号轮数！");
        LotteryBatchDTO dto = new LotteryBatchDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<LotteryResultDetailBO> result = lotteryBatchFacade.viewResult(dto);
        if (null == result){
            return Result.buildSuccessResult(new PageResponse(new ArrayList(), param.getPageNo(), 0L, param.getPageSize()));
        }
        List<LotteryResultDetailPageRsp> lotteryBatchRsps = BeanConvertorUtils.copyList(result.getList(), LotteryResultDetailPageRsp.class);
        return Result.buildSuccessResult(new PageResponse(lotteryBatchRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
    }
}
