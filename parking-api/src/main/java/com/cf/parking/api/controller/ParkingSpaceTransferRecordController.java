package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.aliyun.dingtalkexclusive_1_0.models.GetUserAppVersionSummaryHeaders;
import com.cf.parking.api.request.ParkingSpaceTransferRecordReq;
import com.cf.parking.api.response.ParkingSpaceTransferRecordRsp;
import com.cf.parking.facade.facade.ParkingSpaceTransferRecordFacade;
import com.cf.support.authertication.UserAuthentication;
import com.cf.support.authertication.UserAuthenticationServer;
import com.cf.support.authertication.token.dto.UserSessionDTO;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 车位转赠记录Controller
 * 
 * @author
 * @date 2023-09-05
 */
@UserAuthentication
@Api(tags = "车位转赠记录管理模块——摇号系统")
@Slf4j
@RestController
@RequestMapping("/parkingSpace/transferRecord")
public class ParkingSpaceTransferRecordController
{
    @Resource
    private ParkingSpaceTransferRecordFacade parkingSpaceTransferRecordFacade;
    
    @Resource
    private UserAuthenticationServer userAuthenticationServer;

    private UserSessionDTO getUser() {
        return userAuthenticationServer.getCurrentUser();
    }
    
    /**
     * 查询车位转赠记录列表
     */
    @ApiOperation(value = "查询车位转赠记录列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<ParkingSpaceTransferRecordRsp>> list(@RequestBody ParkingSpaceTransferRecordReq param)
    {
        return null;
    }
    
    
    @ApiOperation(value = "车位转赠", notes = "车位转赠")
    @PostMapping("/transfer")
    public Result transfer(@RequestBody String jobNum){
    	String openId = getUser().getOpenId();
    	parkingSpaceTransferRecordFacade.transfer(openId,jobNum)
    	return Result.buildSuccessResult();
    }

}
