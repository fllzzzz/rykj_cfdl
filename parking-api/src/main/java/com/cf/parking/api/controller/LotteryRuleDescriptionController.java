package com.cf.parking.api.controller;

import com.cf.parking.api.annotation.AdminOptLogTitle;
import com.cf.parking.api.request.LotteryRuleDescriptionOptReq;
import com.cf.parking.api.response.LotteryApplyRsp;
import com.cf.parking.api.response.LotteryRuleDescriptionRsp;
import com.cf.parking.facade.bo.LotteryApplyBO;
import com.cf.parking.facade.bo.LotteryRuleDescriptionBO;
import com.cf.parking.facade.dto.LotteryBlackListOptDTO;
import com.cf.parking.facade.dto.LotteryRuleDescriptionOptDTO;
import com.cf.parking.facade.facade.LotteryRuleDescriptionFacade;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.authertication.AdminUserAuthentication;
import com.cf.support.authertication.token.dto.UserSessionDTO;
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

import javax.annotation.Resource;

/**
 * 摇号规则描述Controller
 *
 * @author
 * @date 2023-10-08
 */
@Api(tags = "摇号规则描述（批次管理页面）——摇号系统")
@Slf4j
@RestController
@RequestMapping("/lottery/description")
public class LotteryRuleDescriptionController {

    @Resource
    private LotteryRuleDescriptionFacade lotteryRuleDescriptionFacade;


    //————————————————PC端————————————————————
    /**
     * 点击查询摇号规则描述信息
     */
    @AdminUserAuthentication
    @ApiOperation(value = "摇号规则描述查询————PC端", notes = "摇号批次页面的描述信息")
    @PostMapping("/info")
    public Result<LotteryRuleDescriptionRsp> info()
    {
        LotteryRuleDescriptionBO bo = lotteryRuleDescriptionFacade.getDescription();
        return Result.buildSuccessResult(BeanConvertorUtils.map(bo,LotteryRuleDescriptionRsp.class));
    }


    /**
     * 编辑摇号规则描述信息
     */
    @AdminUserAuthentication
    @AdminOptLogTitle("编辑摇号规则描述")
    @ApiOperation(value = "摇号规则描述编辑————PC端")
    @PostMapping("/edit")
    public Result edit(@RequestBody LotteryRuleDescriptionOptReq param)
    {
        LotteryRuleDescriptionOptDTO dto = new LotteryRuleDescriptionOptDTO();
        BeanUtils.copyProperties(param,dto);

        Integer result = lotteryRuleDescriptionFacade.edit(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("修改失败，请重试！");

    }


}
