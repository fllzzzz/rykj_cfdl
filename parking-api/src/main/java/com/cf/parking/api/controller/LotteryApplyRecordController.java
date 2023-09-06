package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryApplyRecordReq;
import com.cf.parking.api.response.LotteryApplyRecordPageRsp;
import com.cf.parking.facade.bo.LotteryApplyRecordBO;
import com.cf.parking.facade.dto.LotteryApplyRecordDTO;
import com.cf.parking.facade.facade.LotteryApplyRecordFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
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
@Api(tags = "摇号申请记录模块")
@Slf4j
@RestController
@RequestMapping("/lottery/applyRecord")
public class LotteryApplyRecordController
{
    @Resource
    private LotteryApplyRecordFacade lotteryApplyRecordFacade;

    /**
     * 查询摇号申请记录列表
     */
    @ApiOperation(value = "查询摇号申请记录列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<LotteryApplyRecordPageRsp>>  getApplyRecordList(@RequestBody LotteryApplyRecordReq param)
    {
        LotteryApplyRecordDTO dto = new LotteryApplyRecordDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<LotteryApplyRecordBO> result = lotteryApplyRecordFacade.getApplyRecordList(dto);
        List<LotteryApplyRecordPageRsp> applyRecordPageRsps = BeanConvertorUtils.copyList(result.getList(), LotteryApplyRecordPageRsp.class);
        return Result.buildSuccessResult(new PageResponse(applyRecordPageRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
    }

}
