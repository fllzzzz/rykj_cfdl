package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.ParkingLotAreaOptReq;
import com.cf.parking.api.request.ParkingLotOptReq;
import com.cf.parking.api.request.ParkingLotReq;
import com.cf.parking.api.response.ParkingLotAreaRsp;
import com.cf.parking.api.response.ParkingLotBaseRsp;
import com.cf.parking.api.response.ParkingLotRsp;
import com.cf.parking.facade.bo.ParkingLotAreaBO;
import com.cf.parking.facade.bo.ParkingLotBO;
import com.cf.parking.facade.constant.ParkingSysCodeConstant;
import com.cf.parking.facade.dto.ParkingLotAreaOptDTO;
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
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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
     * 停车场基础信息列表
     */
    @ApiOperation(value = "停车场'区域-区域编号'列表", notes = "其他模块中需要使用停车场信息时调用此方法")
    @PostMapping("/baseList")
    public Result<List<ParkingLotBaseRsp>> baseList()
    {
        Map<String, String> parkingSysCodeMap = ParkingSysCodeConstant.parkingSysCodeMap;

        List<ParkingLotBaseRsp> list = parkingSysCodeMap.entrySet().stream().map(x -> new ParkingLotBaseRsp().setRegion(x.getKey()).setRegionCode(x.getValue())).collect(Collectors.toList());
        return Result.buildSuccessResult(list);
    }

    /**
     * 园区列表
     */
    @ApiOperation(value = "园区列表", notes = "页面左侧园区列表")
    @PostMapping("/areaList")
    public Result<List<ParkingLotAreaRsp>> areaList()
    {
        List<ParkingLotAreaBO> boList = parkingLotFacade.getAreaList();
        List<ParkingLotAreaRsp> areaRsps = BeanConvertorUtils.copyList(boList, ParkingLotAreaRsp.class);
        return Result.buildSuccessResult(areaRsps);
    }

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
     * 新增园区
     */
    @ApiOperation(value = "新增园区", notes = "点击园区新增按钮")
    @PostMapping("/addArea")
    public Result addArea(@RequestBody ParkingLotAreaOptReq param)
    {
        //1.参数验证
        if (StringUtils.isBlank(param.getName())){
            return Result.buildErrorResult("园区名称不能为空！");
        }
        if (CollectionUtils.isEmpty(param.getEntranceList())){
            return Result.buildErrorResult("入口不能为空！");
        }

        //2.参数转换
        ParkingLotAreaOptDTO dto = new ParkingLotAreaOptDTO();
        BeanUtils.copyProperties(param,dto);

        //3.新增处理
        Integer result = parkingLotFacade.addArea(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("新增失败，请重试！");
    }

    /**
     * 修改园区
     */
    @ApiOperation(value = "修改园区", notes = "点击园区右侧的修改按钮")
    @PostMapping("/updateArea")
    public Result updateArea(@RequestBody ParkingLotAreaOptReq param)
    {
        //1.参数验证
        if (StringUtils.isBlank(param.getName())){
            return Result.buildErrorResult("园区名称不能为空！");
        }
        if (CollectionUtils.isEmpty(param.getEntranceList())){
            return Result.buildErrorResult("入口不能为空！");
        }

        //2.参数转换
        ParkingLotAreaOptDTO dto = new ParkingLotAreaOptDTO();
        BeanUtils.copyProperties(param,dto);

        //3.修改处理
        Integer result = parkingLotFacade.updateArea(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("修改失败，请重试！");
    }

    /**
     * 新增停车场
     */
    @ApiOperation(value = "新增停车场", notes = "点击新增按钮")
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
        AssertUtil.checkNull(param.getParentId(), "请选择上级停车场！");
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
