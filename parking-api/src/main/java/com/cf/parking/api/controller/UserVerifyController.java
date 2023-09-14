package com.cf.parking.api.controller;

import com.cf.parking.api.request.UserVerifyOptReq;
import com.cf.parking.api.request.UserVerifyReq;
import com.cf.parking.api.response.UserVerifyRsp;
import com.cf.parking.facade.bo.UserVerifyBO;
import com.cf.parking.facade.dto.UserVerifyDTO;
import com.cf.parking.facade.dto.UserVerifyOptDTO;
import com.cf.parking.facade.facade.UserVerifyFacade;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.authertication.AdminUserAuthentication;
import com.cf.support.authertication.UserAuthentication;
import com.cf.support.authertication.UserAuthenticationServer;
import com.cf.support.authertication.token.dto.UserSessionDTO;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.Authorization;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.util.IOUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.io.*;
import java.util.Base64;
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

    @Resource
    private UserAuthenticationServer userAuthenticationServer;

    private UserSessionDTO getUser() {
        return userAuthenticationServer.getCurrentUser();
    }

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
     * 获取车辆审核详细信息
     */
    @ApiOperation(value = "获取车辆审核详细信息", notes = "点击审核，根据id查询")
    @PostMapping("/info")
    public Result<UserVerifyRsp> getInfo(@RequestBody UserVerifyReq param)
    {
        UserVerifyDTO dto = new UserVerifyDTO();
        BeanUtils.copyProperties(param,dto);

        UserVerifyBO bo = userVerifyFacade.getUserVerify(dto);
        UserVerifyRsp userVerifyRsp = new UserVerifyRsp();
        BeanUtils.copyProperties(bo,userVerifyRsp);
        return Result.buildSuccessResult(userVerifyRsp);
    }

    /**
     * 新增车辆审核
     */
    @UserAuthentication
    @ApiOperation(value = "新增车辆审核", notes = "移动端个人中心模块点击车辆录入")
    @PostMapping("/add")
    public Result add(UserVerifyOptReq param)
    {
        //1.参数校验
        paramVerify(param);

        UserVerifyOptDTO dto;
        try {
            //2.参数生成
            dto = optReq2OptDto(param);
        } catch (IOException e) {
            log.error("用户{}上传车辆信息审核时图片转换异常:{}",getUser().getServerName(),e);
            return Result.buildErrorResult("图片上传异常，请重试！");
        }
        Integer result = userVerifyFacade.add(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("提交审核失败，请重试！");
    }

    private UserVerifyOptDTO optReq2OptDto(UserVerifyOptReq param) throws IOException {
        UserVerifyOptDTO dto = new UserVerifyOptDTO();
        dto.setUserId(1668502647096690L);
//        dto.setUserId(getUser().getUserId());
//        dto.setUserName(getUser().getServerName());
        dto.setPlateNo(param.getPlateNo());
        //2.1.三张图片转base64
        dto.setVehicleImg(getBase64ImgStr(param.getVehicleImg().getInputStream()));
        dto.setDrivingLicenseImg(getBase64ImgStr(param.getDrivingLicenseImg().getInputStream()));
        dto.setDrivingPermitImg(getBase64ImgStr(param.getDrivingPermitImg().getInputStream()));
        return dto;
    }

    //TODO：转换后的字符串无法贴到地址栏内查看
    private String getBase64ImgStr(InputStream inputStream) throws IOException {
        return Base64.getEncoder().encodeToString(IOUtils.toByteArray(inputStream));
    }

    private void paramVerify(UserVerifyOptReq param) {
        AssertUtil.checkNull(param.getPlateNo(),"请输入车牌号！");
        AssertUtil.checkNull(param.getVehicleImg(),"请上传车辆照片！");
        AssertUtil.checkNull(param.getDrivingLicenseImg(),"请上传驾驶证照片！");
        AssertUtil.checkNull(param.getDrivingPermitImg(),"请上传行驶证照片！");
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
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("审核失败，请重试！");
    }

    /**
     * 批量审核车辆
     */
    @ApiOperation(value = "批量审核车辆", notes = "点击批量审核按钮")
    @PostMapping("/batchAudit")
    public Result batchAudit(@RequestBody UserVerifyOptReq param)
    {
        List<Long> ids = param.getIds();
        AssertUtil.checkNull(ids,"请选择要审核的记录！");

        UserVerifyOptDTO dto = new UserVerifyOptDTO();
        BeanUtils.copyProperties(param,dto);

        userVerifyFacade.batchAudit(dto);
        return Result.buildSuccessResult() ;
    }

    //生成的base64
    public static void main(String[] args) throws IOException {
        File file = new File("C:\\Users\\17235\\Pictures\\Saved Pictures\\a.png");
        FileInputStream imageInFile = new FileInputStream(file);
        byte[] imageData = new byte[(int)file.length()];
        imageInFile.read(imageData);
        imageInFile.close();

        String base64Image = Base64.getEncoder()
                .encodeToString(imageData);
        //查看前需要拼接data:image/png;base64,
        System.out.println(base64Image);
    }
}
