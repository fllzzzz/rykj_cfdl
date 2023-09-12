package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryRuleRoundOptReq;
import com.cf.parking.api.request.LotteryRuleRoundReq;
import com.cf.parking.api.response.LotteryBlackListRsp;
import com.cf.parking.api.response.LotteryRuleAssignRsp;
import com.cf.parking.api.response.LotteryRuleRoundBaseRsp;
import com.cf.parking.api.response.LotteryRuleRoundRsp;
import com.cf.parking.facade.bo.LotteryBlackListBO;
import com.cf.parking.facade.bo.LotteryRuleAssignBO;
import com.cf.parking.facade.bo.LotteryRuleRoundBO;
import com.cf.parking.facade.bo.LotteryRuleRoundBaseBO;
import com.cf.parking.facade.dto.LotteryBlackListDTO;
import com.cf.parking.facade.dto.LotteryRuleAssignDTO;
import com.cf.parking.facade.dto.LotteryRuleRoundDTO;
import com.cf.parking.facade.dto.LotteryRuleRoundOptDTO;
import com.cf.parking.facade.facade.LotteryRuleRoundFacade;
import com.cf.parking.services.utils.AssertUtil;
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
 * 摇号规则-轮数Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Api(tags = "摇号规则-轮数管理模块——摇号系统")
@Slf4j
@RestController
@RequestMapping("/lottery/roundRule")
public class LotteryRuleRoundController
{
    @Resource
    private LotteryRuleRoundFacade lotteryRuleRoundFacade;

    /**
     * 查询摇号规则-轮数名称列表
     */
    @ApiOperation(value = "查询摇号规则轮数名称列表", notes = "下拉选择时使用")
    @PostMapping("/baseList")
    public Result<List<LotteryRuleRoundBaseRsp>> baseList()
    {
        List<LotteryRuleRoundBaseBO> baseBOList = lotteryRuleRoundFacade.selectBaseList();
        List<LotteryRuleRoundBaseRsp> lotteryRuleRoundBaseRsps = BeanConvertorUtils.copyList(baseBOList, LotteryRuleRoundBaseRsp.class);
        return Result.buildSuccessResult(lotteryRuleRoundBaseRsps);
    }

    /**
     * 查询摇号规则-轮数列表
     */
    @ApiOperation(value = "查询摇号规则-轮数列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<LotteryRuleRoundRsp>> list(@RequestBody LotteryRuleRoundReq param)
    {

        LotteryRuleRoundDTO dto = new LotteryRuleRoundDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<LotteryRuleRoundBO> result = lotteryRuleRoundFacade.getLotteryRuleRoundList(dto);
        List<LotteryRuleRoundRsp> lotteryRuleRoundRsps = BeanConvertorUtils.copyList(result.getList(), LotteryRuleRoundRsp.class);
        return Result.buildSuccessResult(new PageResponse(lotteryRuleRoundRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
    }


    /**
     * 新增摇号规则-轮数
     */
    @ApiOperation(value = "新增摇号规则-轮数", notes = "点击新增按钮")
    @PostMapping("/add")
    public Result add(@RequestBody LotteryRuleRoundOptReq param)
    {
        //1.参数验证
        paramVerify(param);

        //2.参数转换
        LotteryRuleRoundOptDTO dto = new LotteryRuleRoundOptDTO();
        BeanUtils.copyProperties(param,dto);

        //3.新增处理
        Integer result = lotteryRuleRoundFacade.add(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("新增失败，请重试！");
    }

    /**
     * 修改摇号规则-轮数
     */
    @ApiOperation(value = "修改摇号规则-轮数", notes = "点击修改按钮")
    @PostMapping("/update")
    public Result edit(@RequestBody LotteryRuleRoundOptReq param)
    {
        //1.参数验证
        paramVerify(param);

        //2.参数转换
        LotteryRuleRoundOptDTO dto = new LotteryRuleRoundOptDTO();
        BeanUtils.copyProperties(param,dto);

        //3.修改处理
        Integer result = lotteryRuleRoundFacade.update(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("修改失败，请重试！");
    }

    private void paramVerify(@RequestBody LotteryRuleRoundOptReq param) {
        AssertUtil.checkNull(param.getName(), "轮数不能为空！");
        AssertUtil.checkNull(param.getParkingLotCode(), "停车场不能为空！");
        AssertUtil.checkNull(param.getState(), "状态不能为空！");
    }

    /**
     * 删除摇号规则-轮数
     */
    @ApiOperation(value = "删除摇号规则-轮数", notes = "点击删除按钮")
    @PostMapping("/delete")
    public Result delete(@RequestBody LotteryRuleRoundReq param)
    {
        AssertUtil.checkNull(param.getId(),"请选择要删除的轮数记录！");
        Integer result = lotteryRuleRoundFacade.deleteById(param.getId());
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("删除失败，请重试！");
    }
}
