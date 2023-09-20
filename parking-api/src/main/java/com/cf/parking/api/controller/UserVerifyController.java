package com.cf.parking.api.controller;

import com.cf.parking.api.request.UserVerifyOptReq;
import com.cf.parking.api.request.UserVerifyReq;
import com.cf.parking.api.response.UserVerifyRsp;
import com.cf.parking.dao.po.LotteryBlackListPO;
import com.cf.parking.facade.bo.UserVerifyBO;
import com.cf.parking.facade.dto.UserVerifyDTO;
import com.cf.parking.facade.dto.UserVerifyOptDTO;
import com.cf.parking.facade.facade.UserVerifyFacade;
import com.cf.parking.services.enums.PictureInfoEnum;
import com.cf.parking.services.service.LotteryBlackListService;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.authertication.AdminUserAuthentication;
import com.cf.support.authertication.UserAuthentication;
import com.cf.support.authertication.UserAuthenticationServer;
import com.cf.support.authertication.token.dto.UserSessionDTO;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.poi.util.IOUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

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

    @Resource
    private LotteryBlackListService lotteryBlackListService;

    private UserSessionDTO getUser() {
        UserSessionDTO userSessionDTO = new UserSessionDTO();
        userSessionDTO.setUserId(1668559697477717L);
        userSessionDTO.setServerName("魏慧");
        return userSessionDTO;
//        return userAuthenticationServer.getCurrentUser();
    }



    //————————————————PC端————————————————————
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



    //————————————————小程序端————————————————————

    /**
     * 获取个人车牌号列表
     */
    @ApiOperation(value = "获取个人车辆列表——小程序", notes = "小程序端个人的车辆列表")
    @PostMapping("/vehicleList")
    public Result<List<String>> vehicleList()
    {
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();

        List<String> platNoList = userVerifyFacade.getPlatNoListByUserId(userId);
        return Result.buildSuccessResult(platNoList);
    }

    /**
     * 获取个人车辆审核详细信息
     */
    @ApiOperation(value = "获取个人车辆审核详细信息——小程序", notes = "选择停车场后，自动调用查询")
    @PostMapping("/info")
    public Result<UserVerifyRsp> getInfo(@RequestBody UserVerifyReq param)
    {
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();

        //2.参数校验
        AssertUtil.checkNull(param.getPlatNo(),"请选择车牌号！");

        //3.参数转换
        UserVerifyDTO dto = new UserVerifyDTO();
        BeanUtils.copyProperties(param,dto);
        dto.setUserId(userId);

        //4.查询
        UserVerifyBO bo = userVerifyFacade.getUserVerify(dto);
        UserVerifyRsp userVerifyRsp = new UserVerifyRsp();
        BeanUtils.copyProperties(bo,userVerifyRsp);
        return Result.buildSuccessResult(userVerifyRsp);
    }

    /**
     * 新增车辆审核
     */
    @UserAuthentication
    @ApiOperation(value = "新增车辆审核——小程序", notes = "移动端个人中心模块点击车辆录入")
    @PostMapping("/add")
    public Result add(UserVerifyOptReq param)
    {
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();

        //2.申请资格校验
        //2.1是否为黑名单判断
        LotteryBlackListPO lotteryBlackListPO = lotteryBlackListService.queryBlackUserInfo(userId);
        if (null != lotteryBlackListPO){
            return Result.buildErrorResult("您已被加入黑名单，请联系管理员解除！");
        }

        //2.2判断车辆数量是否大于2
        List<String> platNoList = userVerifyFacade.getPlatNoListByUserId(userId);
        if (CollectionUtils.isNotEmpty(platNoList) && platNoList.size() >= 2){
            return Result.buildErrorResult("最多只能录入2辆车！");
        }


        //3.参数校验
        paramVerify(param);

        UserVerifyOptDTO dto;
        try {
            //4.参数生成（图片转换）
            dto = optReq2OptDto(param);
            dto.setUserId(userId);
            dto.setUserName(user.getServerName());
        } catch (IOException e) {
            log.error("用户{}上传车辆信息审核时图片转换异常:{}",user.getServerName(),e);
            return Result.buildErrorResult("图片上传异常，请重试！");
        }
        Integer result = userVerifyFacade.add(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("提交审核失败，请重试！");
    }

    private UserSessionDTO getUserSessionDTO() {
        UserSessionDTO user = getUser();
        if (ObjectUtils.isEmpty(user)){
            throw new BusinessException("请先登录！");
        }
        return user;
    }

    private UserVerifyOptDTO optReq2OptDto(UserVerifyOptReq param) throws IOException {
        UserVerifyOptDTO dto = new UserVerifyOptDTO();
        dto.setUserId(getUser().getUserId());
        dto.setUserName(getUser().getServerName());
        dto.setPlateNo(param.getPlateNo());
        //4.1三张图片转base64
        dto.setVehicleImg(getBase64ImgStr(param.getVehicleImg()));
        dto.setDrivingLicenseImg(getBase64ImgStr(param.getDrivingLicenseImg()));
        dto.setDrivingPermitImg(getBase64ImgStr(param.getDrivingPermitImg()));
        return dto;
    }

    private String getBase64ImgStr(MultipartFile file) throws IOException {
        if (PictureInfoEnum.CONTENT_TYPE_JPG.getInfo().equals(file.getContentType())){
            return PictureInfoEnum.BASE64_JPG_PRE.getInfo() + Base64.getEncoder().encodeToString(IOUtils.toByteArray(file.getInputStream()));
        }
        return PictureInfoEnum.BASE64_PNG_PRE.getInfo() + Base64.getEncoder().encodeToString(IOUtils.toByteArray(file.getInputStream()));
    }

    private void paramVerify(UserVerifyOptReq param) {
        AssertUtil.checkNull(param.getPlateNo(),"请输入车牌号！");
        AssertUtil.checkNull(param.getVehicleImg(),"请上传车辆照片！");
        picContentTypeVerify(param.getVehicleImg().getContentType());
        AssertUtil.checkNull(param.getDrivingLicenseImg(),"请上传驾驶证照片！");
        picContentTypeVerify(param.getDrivingLicenseImg().getContentType());
        AssertUtil.checkNull(param.getDrivingPermitImg(),"请上传行驶证照片！");
        picContentTypeVerify(param.getDrivingPermitImg().getContentType());
    }

    private void picContentTypeVerify(String contentType) {
        if (!PictureInfoEnum.CONTENT_TYPE_JPG.getInfo().equals(contentType) &&
                !PictureInfoEnum.CONTENT_TYPE_PNG.getInfo().equals(contentType)){
            throw new BusinessException("请上传JPG或PNG类型的图片");
        }
    }

    /**
     * 修改个人车辆审核详细信息
     */
    @ApiOperation(value = "修改个人车辆审核详细信息——小程序", notes = "修改个人车辆审核详细信息")
    @PostMapping("/update")
    public Result update(UserVerifyOptReq param)
    {
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();

        //2.1是否为黑名单判断
        LotteryBlackListPO lotteryBlackListPO = lotteryBlackListService.queryBlackUserInfo(userId);
        if (null != lotteryBlackListPO){
            return Result.buildErrorResult("您已被加入黑名单，请联系管理员解除！");
        }

        //3.参数校验
        AssertUtil.checkNull(param.getId(),"请选择要修改的车辆信息！");
        paramVerify(param);

        //4.参数转换
        UserVerifyOptDTO dto;
        try {
            //4.参数生成（图片转换）
            dto = optReq2OptDto(param);
            dto.setId(param.getId());
            dto.setUserId(userId);
            dto.setUserName(user.getServerName());
        } catch (IOException e) {
            log.error("用户{}上传车辆信息审核时图片转换异常:{}",user.getServerName(),e);
            return Result.buildErrorResult("图片上传异常，请重试！");
        }

        //4.修改
        Integer result = userVerifyFacade.update(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("提交审核失败，请重试！");
    }

    /**
     * 删除个人车辆信息
     */
    @ApiOperation(value = "删除个人车辆信息——小程序", notes = "删除个人车辆信息")
    @PostMapping("/delete")
    public Result<String> delete(@RequestBody UserVerifyReq param)
    {
        AssertUtil.checkNull(param.getId(),"请选择要删除的记录！");
        Integer result = userVerifyFacade.deleteById(param.getId());
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("删除失败，请重试！");
    }
}
