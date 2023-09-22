package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.annotation.AdminOptLogTitle;
import com.cf.parking.api.request.LotteryBlackListOptReq;
import com.cf.parking.api.request.LotteryBlackListReq;
import com.cf.parking.api.response.LotteryBlackListRsp;
import com.cf.parking.facade.bo.LotteryBlackListBO;
import com.cf.parking.facade.dto.LotteryBlackListDTO;
import com.cf.parking.facade.dto.LotteryBlackListOptDTO;
import com.cf.parking.facade.facade.LotteryBlackListFacade;
import com.cf.parking.services.utils.AssertUtil;
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
 * 摇号黑名单Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Api(tags = "摇号黑名单模块——摇号系统")
@Slf4j
@RestController
@RequestMapping("/lottery/blackList")
public class LotteryBlackListController
{
    @Resource
    private LotteryBlackListFacade lotteryBlackListFacade;

    //————————————————PC端————————————————————

    /**
     * 查询摇号黑名单列表
     */
    @AdminUserAuthentication
    @ApiOperation(value = "查询摇号黑名单列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<LotteryBlackListRsp>> list(@RequestBody LotteryBlackListReq param)
    {
        LotteryBlackListDTO dto = new LotteryBlackListDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<LotteryBlackListBO> result = lotteryBlackListFacade.getLotteryBlackList(dto);
        List<LotteryBlackListRsp> lotteryBlackListRsps = BeanConvertorUtils.copyList(result.getList(), LotteryBlackListRsp.class);
        return Result.buildSuccessResult(new PageResponse(lotteryBlackListRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
    }


    /**
     * 新增摇号黑名单
     */
    @AdminOptLogTitle("新增摇号黑名单")
    @AdminUserAuthentication
    @ApiOperation(value = "新增摇号黑名单", notes = "点击新增按钮")
    @PostMapping("/add")
    public Result add(@RequestBody LotteryBlackListOptReq param)
    {
        AssertUtil.checkNull(param.getName(),"请选择员工！");

        LotteryBlackListOptDTO dto = new LotteryBlackListOptDTO();
        BeanUtils.copyProperties(param,dto);

        Integer result = lotteryBlackListFacade.add(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("添加失败，请重试！");
    }

    /**
     * 修改摇号黑名单
     */
    @AdminOptLogTitle("修改摇号黑名单")
    @AdminUserAuthentication
    @ApiOperation(value = "修改摇号黑名单", notes = "点击修改按钮")
    @PostMapping("/update")
    public Result update(@RequestBody LotteryBlackListOptReq param)
    {
        AssertUtil.checkNull(param.getId(),"请选择要修改记录！");
        LotteryBlackListOptDTO dto = new LotteryBlackListOptDTO();
        BeanUtils.copyProperties(param,dto);

        Integer result = lotteryBlackListFacade.update(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("修改失败，请重试！");
    }

    /**
     * 移出摇号黑名单
     */
    @AdminOptLogTitle("移出摇号黑名单")
    @AdminUserAuthentication
    @ApiOperation(value = "移出摇号黑名单", notes = "点击移出按钮")
    @PostMapping("/move")
    public Result remove(@RequestBody LotteryBlackListReq param)
    {
        AssertUtil.checkNull(param.getId(),"请选择要移出黑名单的记录！");
        Integer result = lotteryBlackListFacade.deleteById(param.getId());
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("移出失败，请重试！");
    }
}
