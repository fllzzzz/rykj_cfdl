package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.alibaba.fastjson.JSON;
import com.cf.parking.api.annotation.AdminOptLogTitle;
import com.cf.parking.api.request.LotteryApplyRecordReq;
import com.cf.parking.api.response.LotteryApplyRecordPageRsp;
import com.cf.parking.api.response.LotteryApplyRsp;
import com.cf.parking.facade.bo.LotteryApplyBO;
import com.cf.parking.facade.bo.LotteryApplyRecordBO;
import com.cf.parking.facade.dto.LotteryApplyRecordDTO;
import com.cf.parking.facade.facade.LotteryApplyRecordFacade;
import com.cf.parking.services.service.LotteryBlackListService;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.authertication.UserAuthentication;
import com.cf.support.authertication.token.dto.UserSessionDTO;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.cf.support.result.Result;

import java.util.List;

/**
 * 摇号申请记录Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Api(tags = "摇号申请记录模块——摇号系统")
@Slf4j
@RestController
@RequestMapping("/lottery/applyRecord")
public class LotteryApplyRecordController extends BaseController
{
    @Resource
    private LotteryApplyRecordFacade lotteryApplyRecordFacade;

    @Resource
    private LotteryBlackListService lotteryBlackListService;

   



    //————————————————小程序端————————————————————
    /**
     * 打开页面，摇号信息展示,通知直接跳转，有权限过滤的话打不开
     */
    @ApiOperation(value = "摇号信息展示————小程序", notes = "打开申请页面，摇号信息展示")
    @PostMapping("/info")
    public Result<LotteryApplyRsp>  info()
    {
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();

        //2.个人申请摇号页面信息查询
        LotteryApplyBO applyBO = lotteryApplyRecordFacade.info(userId);
        return Result.buildSuccessResult(BeanConvertorUtils.map(applyBO, LotteryApplyRsp.class));
    }

    /**
     * 申请摇号
     */
    @AdminOptLogTitle("小程序用户申请摇号")
    @UserAuthentication
    @ApiOperation(value = "申请摇号————小程序", notes = "点击申请摇号")
    @PostMapping("/apply")
    public Result  apply(@RequestBody LotteryApplyRecordReq param)
    {
    	log.info("申请摇号入参：{}",JSON.toJSONString(param));
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();

        AssertUtil.checkNull(param.getBatchId(),"请选择摇号报名批次！");

        //2.是否在摇号黑名单内
        if (!ObjectUtils.isEmpty(lotteryBlackListService.queryBlackUserInfo(user.getOpenId()))){
            return Result.buildErrorResult("暂无权限，请联系管理员！");
        }

        //3.当前时间是否在摇号报名时间内
        if (!lotteryApplyRecordFacade.judgeInApplyTime(param.getBatchId())){
            return Result.buildErrorResult("当前未在报名时间内！");
        }


        //4.申请摇号
        Integer result = lotteryApplyRecordFacade.apply(userId,param.getBatchId());
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("申请失败，请重试！");
    }


    /**
     * 取消摇号
     */
    @AdminOptLogTitle("小程序用户取消摇号")
    @UserAuthentication
    @ApiOperation(value = "取消摇号————小程序", notes = "点击取消摇号")
    @PostMapping("/cancel")
    public Result  cancel(@RequestBody LotteryApplyRecordReq param)
    {
    	log.info("取消摇号入参：{}",JSON.toJSONString(param));
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();

        AssertUtil.checkNull(param.getBatchId(),"请选择取消摇号批次！");

        //2.当前时间是否在摇号报名时间内
        if (!lotteryApplyRecordFacade.judgeInApplyTime(param.getBatchId())){
            return Result.buildErrorResult("当前未在报名时间内！");
        }

        //3.申请摇号
        Integer result = lotteryApplyRecordFacade.cancel(userId,param.getBatchId());
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("取消失败，请重试！");
    }


    /**
     * 查询摇号申请记录列表
     */
    @UserAuthentication
    @ApiOperation(value = "查询摇号申请记录列表————小程序", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<LotteryApplyRecordPageRsp>>  getApplyRecordList(@RequestBody LotteryApplyRecordReq param)
    {
    	log.info("查询摇号申请记录入参：{}",JSON.toJSONString(param));
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();
        param.setUserId(userId);

        //2.参数转换
        LotteryApplyRecordDTO dto = new LotteryApplyRecordDTO();
        BeanUtils.copyProperties(param,dto);

        //3.列表查询
        PageResponse<LotteryApplyRecordBO> result = lotteryApplyRecordFacade.getApplyRecordList(dto);
        List<LotteryApplyRecordPageRsp> applyRecordPageRsps = BeanConvertorUtils.copyList(result.getList(), LotteryApplyRecordPageRsp.class);
        return Result.buildSuccessResult(new PageResponse(applyRecordPageRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
    }

}
