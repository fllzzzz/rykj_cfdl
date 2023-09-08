package com.cf.parking.api.controller;

import com.cf.parking.api.request.UserVerifyOptReq;
import com.cf.parking.api.request.UserVerifyReq;
import com.cf.parking.api.response.UserVerifyRsp;
import com.cf.parking.facade.bo.UserVerifyBO;
import com.cf.parking.facade.dto.UserVerifyDTO;
import com.cf.parking.facade.dto.UserVerifyOptDTO;
import com.cf.parking.facade.facade.UserVerifyFacade;
import com.cf.support.authertication.AdminUserAuthentication;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;

/**
 * 车辆审核
 * @author
 * @date 2023/9/7
 */
@Api(tags = "车辆审核模块——摇号系统")
@Slf4j
@RestController
@AdminUserAuthentication
@RequestMapping("/user/verify")
public class UserVerifyController {

    @Resource
    private UserVerifyFacade userVerifyFacade;

    /**
     * 查询车辆审核列表
     */
    @ApiOperation(value = "查询车辆审核列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<UserVerifyRsp>> list(@RequestBody UserVerifyReq param)
    {
        UserVerifyDTO dto = new UserVerifyDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<UserVerifyBO> result = userVerifyFacade.getUserVerifyList(dto);
        List<UserVerifyRsp> verifyRsps = BeanConvertorUtils.copyList(result.getList(), UserVerifyRsp.class);
        return Result.buildSuccessResult(new PageResponse(verifyRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
    }


    /**
     * 获取摇车辆审核详细信息
     */
    @ApiOperation(value = "获取摇车辆审核详细信息", notes = "点击审核，根据id查询")
    @PostMapping("/info")
    public Result<UserVerifyRsp> getInfo(@RequestBody UserVerifyReq param)
    {
        UserVerifyDTO dto = new UserVerifyDTO();
        BeanUtils.copyProperties(param,dto);

        UserVerifyBO bo = userVerifyFacade.getUserVerify(dto);
        UserVerifyRsp userVerifyRsp = new UserVerifyRsp();
        BeanUtils.copyProperties(param,userVerifyRsp);
        return Result.buildSuccessResult(userVerifyRsp);
    }

    /**
     * 新增车辆审核
     */
    @ApiOperation(value = "新增车辆审核", notes = "移动端个人中心模块点击车辆录入")
    @PostMapping("/add")
    public Result add(@RequestBody UserVerifyOptReq param)
    {
        UserVerifyOptDTO dto = new UserVerifyOptDTO();
        BeanUtils.copyProperties(param,dto);
        Integer result = userVerifyFacade.add(dto);
        return result > 0 ?  Result.buildSuccessResult("提交审核成功！") : Result.buildErrorResult("提交审核失败，请重试！");
    }

    /**
     * 审核车辆
     */
    @ApiOperation(value = "审核车辆", notes = "审核界面点击确定按钮")
    @PostMapping("/audit")
    public Result audit(@RequestBody UserVerifyOptReq param)
    {
        UserVerifyOptDTO dto = new UserVerifyOptDTO();
        BeanUtils.copyProperties(param,dto);
        Integer result = userVerifyFacade.audit(dto);
        return result > 0 ?  Result.buildSuccessResult("审核成功！") : Result.buildErrorResult("审核失败，请重试！");
    }

    /**
     * 批量审核车辆
     */
    @ApiOperation(value = "批量审核车辆", notes = "点击批量审核按钮")
    @PostMapping("/batchAudit")
    public Result batchAudit(@RequestBody UserVerifyOptReq param)
    {
        List<Long> ids = param.getIds();
        if (CollectionUtils.isEmpty(ids)){
            return Result.buildErrorResult("请选择要审核的记录！");
        }

        UserVerifyOptDTO dto = new UserVerifyOptDTO();
        BeanUtils.copyProperties(param,dto);

        userVerifyFacade.batchAudit(dto);
        return Result.buildSuccessResult("审核完成！") ;
    }
}
