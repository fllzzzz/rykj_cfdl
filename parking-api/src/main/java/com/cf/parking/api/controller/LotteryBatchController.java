package com.cf.parking.api.controller;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;

import com.alibaba.fastjson.JSON;
import com.cf.parking.api.annotation.AdminOptLogTitle;
import com.cf.parking.api.request.LotteryAllocationReq;
import com.cf.parking.api.request.LotteryBatchOptReq;
import com.cf.parking.api.request.LotteryBatchReq;
import com.cf.parking.api.response.ExportUserVerifyRsp;
import com.cf.parking.api.response.LotteryBatchRsp;
import com.cf.parking.api.response.LotteryResultDetailPageRsp;
import com.cf.parking.api.response.LotteryResultExportRsp;
import com.cf.parking.facade.bo.LotteryBatchBO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.bo.LotteryResultExportBO;
import com.cf.parking.facade.dto.LotteryBatchDTO;
import com.cf.parking.facade.dto.LotteryBatchOptDTO;
import com.cf.parking.facade.facade.LotteryBatchFacade;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.authertication.AdminUserAuthentication;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.ExcelUtiles;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
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

    //————————————————PC端————————————————————

    /**
     * 根据摇号轮数查询车位数量
     */
    @AdminUserAuthentication
    @ApiOperation(value = "根据摇号轮数查询车位数量", notes = "根据摇号轮数查询车位数量")
    @PostMapping("/parkingAmountByRound")
    public Result<Long> parkingAmountByRound(@RequestBody LotteryBatchOptReq param)
    {
    	log.info("根据摇号轮数查询车位数量入参：{}",JSON.toJSONString(param));
        AssertUtil.checkNull(param.getRoundIdArr(),"请选择摇号轮数！");

        Long parkingAmount = lotteryBatchFacade.getParkingAmountByRound(param.getRoundIdArr());
        return Result.buildSuccessResult(parkingAmount);
    }

    /**
     * 查询摇号批次列表
     */
    @AdminUserAuthentication
    @ApiOperation(value = "查询摇号批次列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<LotteryBatchRsp>> list(@RequestBody LotteryBatchReq param)
    {
    	log.info("查询摇号批次列表入参：{}",JSON.toJSONString(param));
        LotteryBatchDTO dto = new LotteryBatchDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<LotteryBatchBO> result = lotteryBatchFacade.getLotteryBatchList(dto);
        List<LotteryBatchRsp> lotteryBatchRsps = BeanConvertorUtils.copyList(result.getList(), LotteryBatchRsp.class);
        Result<PageResponse<LotteryBatchRsp>> resultRsp = Result.buildSuccessResult(new PageResponse(lotteryBatchRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
        return resultRsp;
    }

    /**
     * 新增摇号批次
     * (新增摇号批次时自动生成摇号结果)
     */
    @AdminOptLogTitle("新增摇号批次")
    @AdminUserAuthentication
    @ApiOperation(value = "新增摇号批次", notes = "点击新增按钮")
    @PostMapping("/add")
    public Result add(@RequestBody LotteryBatchOptReq param)
    {
    	log.info("新增摇号批次入参：{}",JSON.toJSONString(param));
        //1.参数校验
        paramVerify(param);

        //2.判断本期车位有效期是否正确（本期车位有效开始日期要晚于上一批车位有效截止日期）
        if (!lotteryBatchFacade.judgeValidStartDateUsable(param.getValidStartDate())){
            return Result.buildErrorResult("车位有效期不能晚于上一期！");
        }

        //3.参数转换
        LotteryBatchOptDTO dto = new LotteryBatchOptDTO();
        BeanUtils.copyProperties(param,dto);

        //4.新增
        Integer result = lotteryBatchFacade.add(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("批次新增失败，请重试！");
    }

    private void paramVerify(LotteryBatchOptReq param) {
        if (null == param.getRoundIdArr() || param.getRoundIdArr().length == 0){
            throw new BusinessException("请选择摇号轮数！");
        }
        AssertUtil.checkNull(param.getBatchNum(),"期号不能为空！");
        AssertUtil.checkNull(param.getParkingAmount(),"车位数量不能为空！");
        AssertUtil.checkNull(param.getApplyStartTime(),"报名开始时间不能为空！");
        AssertUtil.checkNull(param.getApplyEndTime(),"报名结束时间不能为空！");
        AssertUtil.checkNull(param.getValidStartDate(),"车位有效开始日期不能为空！");
        AssertUtil.checkNull(param.getValidEndDate(),"车位有效结束日期不能为空！");
    }

    /**
     * 修改摇号批次
     * (修改前要判断是否已通知)
     */
    @AdminOptLogTitle("修改摇号批次信息")
    @AdminUserAuthentication
    @ApiOperation(value = "修改摇号批次", notes = "点击修改按钮")
    @PostMapping("/update")
    public Result edit(@RequestBody LotteryBatchOptReq param)
    {
    	log.info("修改摇号批次入参：{}",JSON.toJSONString(param));
    	
        //1.参数校验
        paramVerify(param);

        //2.判断本期车位有效期是否正确（本期车位有效开始日期要晚于上一批车位有效截止日期）
        if (!lotteryBatchFacade.judgeValidStartDateUsable(param.getValidStartDate())){
            return Result.buildErrorResult("车位有效期不能晚于上一期！");
        }

        //2.参数转换
        LotteryBatchOptDTO dto = new LotteryBatchOptDTO();
        BeanUtils.copyProperties(param,dto);

        Integer result = lotteryBatchFacade.update(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("批次修改失败，请重试！");
    }

    /**
     * 删除摇号批次
     * (删除前要判断是否已通知)
     */
    @AdminOptLogTitle("删除摇号批次")
    @AdminUserAuthentication
    @ApiOperation(value = "删除摇号批次", notes = "点击删除按钮")
	@PostMapping("/delete")
    public Result delete(@RequestBody LotteryBatchReq param)
    {
    	log.info("删除摇号批次入参：{}",JSON.toJSONString(param));
        AssertUtil.checkNull(param.getId(),"请选择要删除的批次！");

        Integer result = lotteryBatchFacade.deleteById(param.getId());
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("批次删除失败，请重试！");
    }

    /**
     * 下发钉钉通知
     */
    @AdminOptLogTitle("通知钉钉用户摇号批次信息")
    @AdminUserAuthentication
    @ApiOperation(value = "下发钉钉通知", notes = "点击通知按钮")
    @PostMapping("/notify")
    public Result notify(@RequestBody LotteryBatchReq param)
    {
    	log.info("下发钉钉通知入参：{}",JSON.toJSONString(param));
        AssertUtil.checkNull(param.getId(),"请选择要通知的批次！");

        Integer result = lotteryBatchFacade.notifyAllUserByBatchId(param.getId());
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("通知失败，请重试！");
    }

    
    /**
     * 给本批次未中签人员分配停车场
     * (修改前要判断是否已通知)
     */
    @AdminOptLogTitle("给本批次未中签人员分配停车场")
    @AdminUserAuthentication
    @ApiOperation(value = "未中签人员分配停车场", notes = "未中签人员分配停车场")
    @PostMapping("/allocationPark")
    public Result allocationPark(@RequestBody LotteryAllocationReq param)
    {
    	log.info("未中签人员分配停车场入参：{}",JSON.toJSONString(param));
    	
        //1.参数校验
    	AssertUtil.checkNull(param, "参数不能为空");
    	AssertUtil.checkNull(param.getId(), "批次ID不能为空");
    	AssertUtil.checkNull(param.getParkingCode(), "停车场不能为空");

        lotteryBatchFacade.allocationPark(param.getId(),param.getParkingCode());
        return Result.buildSuccessResult() ;
    }


    /**
     * 结果查看
     */
    @AdminUserAuthentication
    @ApiOperation(value = "结果查看", notes = "点击结果查看按钮")
    @PostMapping("/viewResult")
    public Result<PageResponse<LotteryResultDetailPageRsp>> viewResult(@RequestBody LotteryBatchReq param)
    {
    	log.info("结果查看入参：{}",JSON.toJSONString(param));
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


    /**
     * 结果导出
     */
    @AdminUserAuthentication
    @ApiOperation(value = "结果导出", notes = "点击结果导出按钮")
    @PostMapping("/exportResult")
    public void  exportResult(@RequestBody LotteryBatchReq param, HttpServletResponse response)
    {
        log.info("结果导出入参：{}",JSON.toJSONString(param));
        AssertUtil.checkNull(param.getId(),"请选择摇号批次！");
        List<LotteryResultExportBO>  boList= lotteryBatchFacade.exportResult(param.getId());
        List<LotteryResultExportRsp> lotteryResultExportRsps = BeanConvertorUtils.copyList(boList, LotteryResultExportRsp.class);
        ExcelUtiles.exportExcel(lotteryResultExportRsps, "摇号结果", "摇号结果", ExportUserVerifyRsp.class, "摇号结果.xlsx", response);
    }
}
