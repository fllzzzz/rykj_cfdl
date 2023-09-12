package com.cf.parking.api.controller;


import com.alibaba.fastjson.JSON;
import com.cf.parking.api.annotation.PreventRepeat;
import com.cf.parking.api.request.*;
import com.cf.parking.api.response.*;
import com.cf.parking.facade.bo.ScoreRecordBO;
import com.cf.parking.facade.bo.UserCommonAddressBO;
import com.cf.parking.facade.bo.UserLoginBO;
import com.cf.parking.facade.bo.UserProfileBO;
import com.cf.parking.facade.dto.CommonAddressDTO;
import com.cf.parking.facade.dto.DriverEvaluateDTO;
import com.cf.parking.facade.dto.EvaluateDTO;
import com.cf.parking.facade.dto.UserProfileDTO;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.parking.facade.facade.ScoreRecordFacade;
import com.cf.parking.facade.facade.UserFacade;
import com.cf.parking.services.service.UserCommonAddressService;
import com.cf.parking.services.utils.EmptyUtils;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.authertication.UserAuthentication;
import com.cf.support.authertication.UserAuthenticationServer;
import com.cf.support.authertication.token.dto.UserSessionDTO;
import com.cf.support.result.PageRequest;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.PhoneCheckUtil;
import com.cf.support.utils.RedisUtil;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;

@Slf4j
@RestController
@RequestMapping("/user")
@Api(tags = "用户接口")
public class UserController {
    @Resource
    private UserCommonAddressService userCommonAddressService;

    @Resource
    private UserFacade userFacade;

    @Resource
    private UserAuthenticationServer userAuthenticationServer;

    @Resource
    private ScoreRecordFacade scoreRecordFacade;
    @Resource
    private RedisUtil redisUtil;

    @Value("${spring.profiles.active}")
    private String active;
    private UserSessionDTO getUser() {
        return userAuthenticationServer.getCurrentUser();
    }

    /**
     * jsapi鉴权
     *
     * @return
     */
    @ApiOperation(value = "jsapi鉴权", notes = "jsapi鉴权")
    @PostMapping("/jsapi/authentication")
    public Result<AuthenticationResp> authentication() {
    	log.info("jsapi授权参数");
    	return Result.buildSuccessResult(BeanConvertorUtils.map(userFacade.getAuthenticationInfo(), AuthenticationResp.class));
    }


    /**
     * 钉钉授权登录
     *
     * @param param
     * @return
     */
    @PostMapping("/loginByCode")
    public Result<UserLoginResp> loginByCode(@RequestBody LoginByCodeReq param) {
    	log.info("code登陆：{}",JSON.toJSONString(param));
        Result<UserLoginBO> facadeResult = userFacade.loginByCode(param.getCode());
        if (!facadeResult.isSuccess()) {
            return BeanConvertorUtils.map(facadeResult, Result.class);
        }
        return Result.buildSuccessResult(BeanConvertorUtils.map(facadeResult.getData(), UserLoginResp.class));
    }
//    /**
//     * 自动塞入app版本号
//     *
//     * @param appVersion app版本号
//     * @return
//     */
//    @ApiOperation(value = "自动塞入app版本号", notes = "自动塞入app版本号")
//    @PostMapping("/change/version")
//    public Result changeAppVersion(@RequestBody Integer appVersion) {
//        if(!"prod".equals(active)){
//            redisUtil.set(RedisConstant.PARKING_APP_VERSION,appVersion);
//        }
//        return Result.buildSuccessResult();
//    }
    /**
     * 司机评价
     *
     * @param param
     * @return
     */
    @ApiOperation(value = "司机评价", notes = "司机评价")
    @UserAuthentication
    @PostMapping("/evaluate/drive")
    @PreventRepeat
    public Result driverEvaluate(@RequestBody DriverEvaluateReq param) {
        DriverEvaluateDTO driverEvaluateDTO = BeanConvertorUtils.map(param, DriverEvaluateDTO.class);
        if (ObjectUtils.isEmpty(param.getLevel()) || ObjectUtils.isEmpty(param.getParkingOrderId()) || ObjectUtils.isEmpty(param.getUserId()) || StringUtils.isNotEmpty(param.getEvaluateDesc()) && param.getEvaluateDesc().length() > 200) {
            return Result.buildResult(BizResultCodeEnum.PARAM_ERROR);
        }

        // 校验评分
        if (param.getLevel() > 5 || param.getLevel() <= 0) {
            return Result.buildResult(BizResultCodeEnum.PARAM_ERROR);
        }
        Long driverUserId = getUser().getUserId();
        driverEvaluateDTO.setLevel(param.getLevel()).setDriverUserId(driverUserId);
        return userFacade.driverEvaluate(driverEvaluateDTO);
    }

    /**
     * 获取其他人信息
     *
     * @param param
     * @return
     */
    @ApiOperation(value = "获取他人信息", notes = "获取他人信息")
    @UserAuthentication
    @PostMapping("/otherprofile")
    public Result otherProfile(@RequestBody UserProfileReq param) {
        // userId没传
        if (ObjectUtils.isEmpty(param.getUserId())) {
            return Result.buildResult(BizResultCodeEnum.PARAM_ERROR);
        }
        Result<UserProfileBO> result = userFacade.otherProfile(param.getUserId());
        if (result.isSuccess() && result.getData() != null) {
            return Result.buildSuccessResult(BeanConvertorUtils.map(result.getData(), UserProfileRsp.class));
        }
        return BeanConvertorUtils.map(result, Result.class);
    }

    /**
     * 修改个人信息
     * @param param
     * @return
     */
    @ApiOperation(value = "修改个人信息", notes = "修改个人信息")
    @UserAuthentication
    @PostMapping("/changeProfile")
    @PreventRepeat
    public Result changeProfile(@RequestBody UserProfileReq param) {
        if ((StringUtils.isNotEmpty(param.getLivePlace()) &&
                param.getLivePlace().length() > 100) ||
                (StringUtils.isEmpty(param.getMobile())
                        || !PhoneCheckUtil.isPhoneLegal(param.getMobile())) ||
                (StringUtils.isNotEmpty(param.getPlateNo()) && param.getPlateNo().length() > 20)) {
            return Result.buildResult(BizResultCodeEnum.PARAM_ERROR);
        }
        UserProfileDTO userProfile = BeanConvertorUtils.map(param, UserProfileDTO.class);
        userProfile.setUserId(getUser().getUserId());
        return userFacade.changeProfile(userProfile);
    }

    @ApiOperation(value = "乘客评价接口", notes = "乘客评价接口")
    @UserAuthentication
    @PreventRepeat
    @PostMapping("/evaluate/passenger")
    public Result passengerEvaluate(@RequestBody EvaluateReq param) {
        EmptyUtils.emptyParkingOrderId(param.getParkingOrderId());
        EmptyUtils.emptyLevel(param.getLevel());
        EvaluateDTO evaluateDTO = BeanConvertorUtils.map(param, EvaluateDTO.class);
        evaluateDTO.setUserId(getUser().getUserId());
        return userFacade.passengerEvaluate(evaluateDTO);
    }

    @ApiOperation(value = "查询个人信息接口", notes = "查询个人信息接口")
    @PostMapping("/profile")
    @UserAuthentication
    public Result<UserProfileRsp> getUserProfile() {
        UserProfileBO userProfile = userFacade.getUserProfile(getUser().getUserId());
        return Result.buildSuccessResult(BeanConvertorUtils.map(userProfile, UserProfileRsp.class));
    }

    @ApiOperation(value = "查询积分记录接口", notes = "查询积分记录接口")
    @UserAuthentication
    @PostMapping("/scores")
    public Result<PageResponse<ScoreRecordRsp>> getScoresPage(@RequestBody PageRequest param) {
        PageResponse<ScoreRecordBO> scoresPage = scoreRecordFacade.getScoresPage(getUser().getUserId(), param);
        if (scoresPage.getTotal() == 0) {
            return PageUtils.emptyPageResult(scoresPage);
        }
        List<ScoreRecordRsp> scoreRecordRsps = BeanConvertorUtils.copyList(scoresPage.getList(), ScoreRecordRsp.class);
        return PageUtils.pageResult(scoresPage, scoreRecordRsps);
    }

    @ApiOperation(value = "新增常用地址接口", notes = "新增常用地址接口")
    @UserAuthentication
    @PostMapping("/addAddress")
    public Result addAddress(@RequestBody CommonAddressReq param) {
        CommonAddressDTO commonAddressDTO = BeanConvertorUtils.map(param, CommonAddressDTO.class);
        return userCommonAddressService.addAddress(commonAddressDTO, getUser().getUserId());
    }

    @ApiOperation(value = "删除常用地址接口", notes = "删除常用地址接口")
    @UserAuthentication
    @PostMapping("/delAddress")
    public Result delAddress(@RequestBody UserAddressIdReq param) {
        return userCommonAddressService.delAddress(param.getUserCommonAddressId(), getUser().getUserId());
    }

    @ApiOperation(value = "获取常用地址接口", notes = "获取常用地址接口")
    @UserAuthentication
    @PostMapping("/getAddress")
    public Result getAddress() {
        List<UserCommonAddressBO> commonAddressBOS = userCommonAddressService.getAddress(getUser().getUserId());
        return Result.buildSuccessResult(BeanConvertorUtils.copyList(commonAddressBOS, UserCommonAddressRsp.class));
    }
}
