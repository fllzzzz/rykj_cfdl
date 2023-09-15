package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryResultReq;
import com.cf.parking.api.request.UserSpacePageReq;
import com.cf.parking.api.response.LotteryResultDetailPageRsp;
import com.cf.parking.api.response.LotteryResultPageRsp;
import com.cf.parking.api.response.UserSpaceRsp;
import com.cf.parking.facade.bo.LotteryResultBO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.bo.UserSpaceBO;
import com.cf.parking.facade.dto.LotteryResultDTO;
import com.cf.parking.facade.dto.UserSpaceDTO;
import com.cf.parking.facade.facade.LotteryResultFacade;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.authertication.AdminUserAuthentication;
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
 * 摇号结果Controller
 * 
 * @author
 * @date 2023-09-05
 */
@AdminUserAuthentication
@Api(tags = "摇号结果模块——摇号系统")
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
    @ApiOperation(value = "查询摇号结果列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<LotteryResultPageRsp>> list(@RequestBody LotteryResultReq param)
    {
        LotteryResultDTO dto = new LotteryResultDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<LotteryResultBO> result = lotteryResultFacade.getLotteryResultList(dto);
        List<LotteryResultPageRsp> lotteryResultRsps = BeanConvertorUtils.copyList(result.getList(), LotteryResultPageRsp.class);
        return Result.buildSuccessResult(new PageResponse(lotteryResultRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
    }

    /**
     * 开始摇号
     */
    @ApiOperation(value = "开始摇号", notes = "点击开始摇号按钮")
    @PostMapping("/start")
    public Result start(@RequestBody LotteryResultReq param)
    {
    	AssertUtil.checkNull(param.getId(), "id不能为空");
    	lotteryResultFacade.lottery(param.getId());
        return Result.buildSuccessResult();
    }

    /**
     * 结果确认
     */
    @ApiOperation(value = "结果确认", notes = "点击结果确认按钮")
    @PostMapping("/confirm")
    public Result confirm(@RequestBody LotteryResultReq param)
    {
    	lotteryResultFacade.confirm(param.getId());
        return Result.buildSuccessResult();
    }

    /**
     * 结果发布
     */
    @ApiOperation(value = "结果发布", notes = "点击结果发布按钮")
	@PostMapping("/publish")
    public Result publish(@RequestBody LotteryResultReq param)
    {
        return Result.buildSuccessResult();
    }

    /**
     * 结果归档
     *
     */
    @ApiOperation(value = "结果归档", notes = "点击结果归档按钮")
    @PostMapping("/archive")
    public Result archive(@RequestBody LotteryResultReq param)
    {
        AssertUtil.checkNull(param.getId(),"请选择要归档的摇号结果！");

        Integer result = lotteryResultFacade.archive(param.getId());
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("归档失败，请重试！");
    }

    /**
     * 摇号结果查询
     */
    @ApiOperation(value = "摇号结果查询", notes = "摇号结果查询")
    @PostMapping("/lotteryResult")
    public Result<PageResponse<LotteryResultDetailPageRsp>> lotteryResult(@RequestBody LotteryResultReq param)
    {
        AssertUtil.checkNull(param.getId(),"请选择要查询的摇号结果记录！");
        LotteryResultDTO dto = new LotteryResultDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<LotteryResultDetailBO> result = lotteryResultFacade.lotteryResult(dto);
        List<LotteryResultDetailPageRsp> detailPageRsps = BeanConvertorUtils.copyList(result.getList(), LotteryResultDetailPageRsp.class);
        return PageUtils.pageResult(result,detailPageRsps);
    }


    /**
     * 确认结果查询（用户车位表中的记录）
     */
    @ApiOperation(value = "确认结果查询", notes = "确认结果查询")
    @PostMapping("/confirmResult")
    public Result<List<UserSpaceRsp>> confirmResult(@RequestBody UserSpacePageReq param)
    {
        AssertUtil.checkNull(param.getBatchNum(),"确认结果的期号不能为空！");
        AssertUtil.checkNull(param.getBatchNum(),"确认结果的轮数不能为空！");
        UserSpaceDTO dto = new UserSpaceDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<UserSpaceBO> result = lotteryResultFacade.confirmResult(dto);
        List<UserSpaceRsp> userSpaceRsps = BeanConvertorUtils.copyList(result.getList(), UserSpaceRsp.class);
        return PageUtils.pageResult(result,userSpaceRsps);
    }
}
