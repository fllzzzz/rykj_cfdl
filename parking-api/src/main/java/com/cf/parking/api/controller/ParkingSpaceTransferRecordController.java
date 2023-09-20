package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.alibaba.fastjson.JSON;
import com.cf.parking.api.request.ParkingSpaceTransferRecordReq;
import com.cf.parking.api.request.TransferReq;
import com.cf.parking.api.response.ParkingSpaceTransferRecordRsp;
import com.cf.parking.facade.bo.ParkingSpaceTransferRecordBO;
import com.cf.parking.facade.dto.ParkingSpaceTransferRecordDTO;
import com.cf.parking.facade.facade.ParkingSpaceTransferRecordFacade;
import com.cf.support.authertication.UserAuthentication;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.authertication.UserAuthenticationServer;
import com.cf.support.authertication.token.dto.UserSessionDTO;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 车位转赠记录Controller
 * 
 * @author
 * @date 2023-09-05
 */
//@UserAuthentication
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
    @ApiOperation(value = "查询车位转赠记录列表————小程序", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<ParkingSpaceTransferRecordRsp>> list(@RequestBody ParkingSpaceTransferRecordReq param)
    {
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();
        param.setUserId(userId);
        log.info("转赠记录查询参数：{}",JSON.toJSONString(param));
        //2.参数转换
        ParkingSpaceTransferRecordDTO dto = new ParkingSpaceTransferRecordDTO();
        BeanUtils.copyProperties(param,dto);

        //3.列表查询
        PageResponse<ParkingSpaceTransferRecordBO> result = parkingSpaceTransferRecordFacade.getParkingSpaceTransferRecordList(dto);
        List<ParkingSpaceTransferRecordRsp> transferRecordRsps = BeanConvertorUtils.copyList(result.getList(), ParkingSpaceTransferRecordRsp.class);
        return PageUtils.pageResult(result,transferRecordRsps);
    }

    private UserSessionDTO getUserSessionDTO() {
        UserSessionDTO user = getUser();
        if (ObjectUtils.isEmpty(user)){
            throw new BusinessException("请先登录！");
        }
        return user;
    }


    @ApiOperation(value = "车位转赠————小程序", notes = "车位转赠")
    @PostMapping("/transfer")
    public Result transfer( @RequestBody TransferReq req){
    	AssertUtil.checkNull(req, "参数不存在");
    	AssertUtil.checkNull(req.getJobNum(), "受让人不存在");
    	log.info("开始转赠：{}",req.getJobNum());
        UserSessionDTO user = getUserSessionDTO();
        String openId = user.getOpenId();
    	parkingSpaceTransferRecordFacade.transfer(openId,req.getJobNum());
    	return Result.buildSuccessResult();
    }

}
