package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.ParkingLotOptReq;
import com.cf.parking.api.request.ParkingLotReq;
import com.cf.parking.api.response.ParkingLotRsp;
import com.cf.parking.facade.bo.ParkingLotBO;
import com.cf.parking.facade.dto.ParkingLotDTO;
import com.cf.parking.facade.dto.ParkingLotOptDTO;
import com.cf.parking.facade.facade.ParkingLotFacade;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.PageUtils;
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
 * 停车场Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Api(tags = "停车场管理模块——摇号系统")
@Slf4j
@RestController
@RequestMapping("/parking/lot")
public class ParkingLotController
{
    @Resource
    private ParkingLotFacade parkingLotFacade;

    /**
     * 查询停车场列表
     */
    @ApiOperation(value = "查询停车场列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<ParkingLotRsp>> list(@RequestBody ParkingLotReq param)
    {
        ParkingLotDTO dto = new ParkingLotDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<ParkingLotBO> result = parkingLotFacade.getParkingLotList(dto);
        if (result.getTotal() == 0) {
            return PageUtils.emptyPageResult(result);
        }
        List<ParkingLotRsp> lotteryRuleRoundRsps = BeanConvertorUtils.copyList(result.getList(), ParkingLotRsp.class);
        return PageUtils.pageResult(result,lotteryRuleRoundRsps);
    }


    /**
     * 新增停车场
     */
    @ApiOperation(value = "新增摇号规则-轮数", notes = "点击新增按钮")
    @PostMapping("/add")
    public Result add(@RequestBody ParkingLotOptReq param)
    {
        //1.参数验证
        paramVerify(param);

        //2.参数转换
        ParkingLotOptDTO dto = new ParkingLotOptDTO();
        BeanUtils.copyProperties(param,dto);

        //3.新增处理
        Integer result = parkingLotFacade.add(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("新增失败，请重试！");
    }

    /**
     * 修改停车场
     */
    @ApiOperation(value = "修改停车场", notes = "点击修改按钮")
    @PostMapping("/update")
    public Result update(@RequestBody ParkingLotOptReq param)
    {
        //1.参数验证
        paramVerify(param);

        //2.参数转换
        ParkingLotOptDTO dto = new ParkingLotOptDTO();
        BeanUtils.copyProperties(param,dto);

        //3.新增处理
        Integer result = parkingLotFacade.update(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("修改失败，请重试！");
    }

    private void paramVerify(@RequestBody ParkingLotOptReq param) {
        AssertUtil.checkNull(param.getRegion(), "区域不能为空！");
        AssertUtil.checkNull(param.getAmount(), "车位数量不能为空！");
        AssertUtil.checkNull(param.getType(), "类型不能为空！");
    }

    /**
     * 删除停车场
     */
    @ApiOperation(value = "删除停车场", notes = "点击删除按钮")
    @PostMapping("/delete")
    public Result delete(@RequestBody ParkingLotReq param)
    {
        AssertUtil.checkNull(param.getId(), "请选择要删除的停车场记录！");
        Integer result = parkingLotFacade.deleteById(param.getId());
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("删除失败，请重试！");
    }
}
