package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.LotteryBatchPageReq;
import com.cf.parking.api.response.LotteryApplyRecordPageRsp;
import com.cf.parking.api.response.LotteryBatchPageRsp;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.facade.bo.LotteryApplyRecordBO;
import com.cf.parking.facade.bo.LotteryBatchBO;
import com.cf.parking.facade.dto.LotteryBatchDTO;
import com.cf.parking.facade.facade.LotteryBatchFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
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
    @PostMapping("/list")
    public Result<PageResponse<LotteryBatchPageRsp>> list(@RequestBody LotteryBatchPageReq param)
    {
        LotteryBatchDTO dto = new LotteryBatchDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<LotteryBatchBO> result = lotteryBatchFacade.getLotteryBatchList(dto);
        List<LotteryBatchPageRsp> lotteryBatchPageRsps = BeanConvertorUtils.copyList(result.getList(), LotteryBatchPageRsp.class);
        return Result.buildSuccessResult(new PageResponse(lotteryBatchPageRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
    }


    /**
     * 获取摇号批次详细信息
     */
    @PostMapping("/1")
    public Result getInfo(@RequestBody Long id)
    {
        return null;
    }

    /**
     * 新增摇号批次
     */
    @PostMapping("/2")
    public Result add(@RequestBody LotteryBatchPO lotteryBatchPO)
    {
        return null;
    }

    /**
     * 修改摇号批次
     */
    @PostMapping("/3")
    public Result edit(@RequestBody LotteryBatchPO lotteryBatchPO)
    {
        return null;
    }

    /**
     * 删除摇号批次
     */
	@PostMapping("/4")
    public Result remove(@RequestBody Long[] ids)
    {
        return null;
    }
}
