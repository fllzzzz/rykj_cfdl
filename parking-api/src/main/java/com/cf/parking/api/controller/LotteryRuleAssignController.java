package com.cf.parking.api.controller;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;
import com.alibaba.fastjson.JSON;
import com.cf.parking.api.annotation.AdminOptLogTitle;
import com.cf.parking.api.request.LotteryRuleAssignOptReq;
import com.cf.parking.api.request.LotteryRuleAssignReq;
import com.cf.parking.api.response.*;
import com.cf.parking.dao.po.EmployeePO;
import com.cf.parking.facade.bo.DepartmentTreeBO;
import com.cf.parking.facade.bo.LotteryRuleAssignBO;
import com.cf.parking.facade.bo.LotteryRuleAssignExportBO;
import com.cf.parking.facade.dto.LotteryRuleAssignDTO;
import com.cf.parking.facade.dto.LotteryRuleAssignOptDTO;
import com.cf.parking.facade.facade.LotteryRuleAssignFacade;
import com.cf.parking.services.service.DepartmentService;
import com.cf.parking.services.service.EmployeeService;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.authertication.AdminUserAuthentication;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.ExcelUtiles;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 摇号规则-停车场分配Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Api(tags = "摇号规则-停车场分配模块——摇号系统")
@Slf4j
@RestController
@RequestMapping("/lottery/assignRule")
public class LotteryRuleAssignController
{
    @Resource
    private LotteryRuleAssignFacade lotteryRuleAssignFacade;

    @Resource
    private DepartmentService departmentService;

    @Resource
    private EmployeeService employeeService;

    //————————————————PC端————————————————————

    /**
     * 人员列表
     */
    @ApiOperation(value = "人员列表", notes = "停车场分配模块中需要使用到人员列表")
    @PostMapping("/userList")
    public Result<List<UserProfileBaseRsp>> userList()
    {
    	List<EmployeePO> poList = employeeService.queryAllEmployee();
        List<UserProfileBaseRsp> rspList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(poList)){
            rspList = poList.stream().map(x -> new UserProfileBaseRsp().setName(x.getName()).setCode(x.getEmplNo())).collect(Collectors.toList());
        }
        return Result.buildSuccessResult(rspList);
    }


    /**
     * 部门树状结构
     */
    @ApiOperation(value = "部门树状结构", notes = "停车场分配模块中需要使用到部门树状结构")
    @PostMapping("/departmentTree")
    public Result<List<DepartmentTreeRsp>> departmentTree()
    {
        List<DepartmentTreeBO> boList = departmentService.departmentTree();
        return Result.buildSuccessResult(BeanConvertorUtils.copyList(boList,DepartmentTreeRsp.class));
    }


    /**
     * 查询摇号规则-停车场分配列表
     */
    @AdminUserAuthentication
    @ApiOperation(value = "查询摇号规则-停车场分配列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<LotteryRuleAssignRsp>> list(@RequestBody LotteryRuleAssignReq param)
    {
        LotteryRuleAssignDTO dto = new LotteryRuleAssignDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<LotteryRuleAssignBO> result = lotteryRuleAssignFacade.getLotteryRuleAssignList(dto);
        List<LotteryRuleAssignRsp> lotteryRuleAssignRsps = BeanConvertorUtils.copyList(result.getList(), LotteryRuleAssignRsp.class);
        return Result.buildSuccessResult(new PageResponse(lotteryRuleAssignRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
    }


    /**
     * 新增摇号规则-停车场分配
     */
    @AdminOptLogTitle("新增摇号规则-停车场分配规则")
    @AdminUserAuthentication
    @ApiOperation(value = "新增摇号规则-停车场分配", notes = "点击新增按钮")
    @PostMapping("/add")
    public Result add(@RequestBody LotteryRuleAssignOptReq param)
    {
        //1.参数校验
        paramVerify(param);

        //2.参数转换
        LotteryRuleAssignOptDTO dto = new LotteryRuleAssignOptDTO();
        BeanUtils.copyProperties(param,dto);

        //3.新增处理
        Integer result = lotteryRuleAssignFacade.add(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("新增失败，请重试！");
    }

    /**
     * 修改摇号规则-停车场分配
     */
    @AdminOptLogTitle("修改摇号规则-停车场分配规则")
    @AdminUserAuthentication
    @ApiOperation(value = "修改摇号规则-停车场分配", notes = "点击修改按钮")
    @PostMapping("/update")
    public Result update(@RequestBody LotteryRuleAssignOptReq param) {
        //1.参数校验
        AssertUtil.checkNull(param.getId(), "请选择分配规则记录！");
        paramVerify(param);

        //2.参数转换
        LotteryRuleAssignOptDTO dto = new LotteryRuleAssignOptDTO();
        BeanUtils.copyProperties(param, dto);

        //3.修改处理
        Integer result = lotteryRuleAssignFacade.update(dto);
        return result > 0 ? Result.buildSuccessResult() : Result.buildErrorResult("修改失败，请重试！");

    }

    private void paramVerify(@RequestBody LotteryRuleAssignOptReq param) {
        AssertUtil.checkNull(param.getRoundId(), "请选择轮数！");
        AssertUtil.checkNull(param.getType(), "请选择分配类型！");
        AssertUtil.checkNull(param.getCodeArr(), "请选择名称！");
    }

    /**
     * 删除摇号规则-停车场分配
     */
    @AdminOptLogTitle("删除摇号规则-停车场分配规则")
    @AdminUserAuthentication
    @ApiOperation(value = "删除摇号规则-停车场分配", notes = "点击删除按钮")
    @PostMapping("/delete")
    public Result delete(@RequestBody LotteryRuleAssignReq param)
    {
        AssertUtil.checkNull(param.getId(),"请选择要删除的分配记录！");
        Integer result = lotteryRuleAssignFacade.deleteById(param.getId());
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("删除失败，请重试！");
    }


    /**
     * 根据roundId查询停车场名称
     */
    @AdminUserAuthentication
    @ApiOperation(value = "根据roundId查询停车场名称", notes = "根据roundId查询停车场名称")
    @PostMapping("/regionByRoundId")
    public Result<String> parkingLotRegionByRoundId(@RequestBody LotteryRuleAssignReq param)
    {
        AssertUtil.checkNull(param.getRoundId(),"请选择摇号轮数！");
        String region = lotteryRuleAssignFacade.getParkingLotRegionByRoundId(param.getRoundId());
        return Result.buildSuccessResult(region);
    }

    /**
     * 人员导出
     */
    @AdminUserAuthentication
    @ApiOperation(value = "人员导出", notes = "人员导出")
    @PostMapping("/exportEmployee")
    public void  exportEmployee(@RequestBody LotteryRuleAssignReq param, HttpServletResponse response)
    {
        log.info("人员导出：{}", JSON.toJSONString(param));
        AssertUtil.checkNull(param.getId(),"请选择要导出的记录！");
        List<LotteryRuleAssignExportBO>  boList= lotteryRuleAssignFacade.exportEmployee(param.getId());
        List<LotteryRuleAssignExportRsp> exportRsps = BeanConvertorUtils.copyList(boList, LotteryRuleAssignExportRsp.class);
        ExcelUtiles.exportExcel(exportRsps, "分配人员名单", "分配人员名单", LotteryRuleAssignExportRsp.class, "分配人员名单.xlsx", response);
    }
}
