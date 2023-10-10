package com.cf.parking.api.controller;

import javax.annotation.Resource;
import com.alibaba.fastjson.JSON;
import com.cf.parking.api.annotation.AdminOptLogTitle;
import com.cf.parking.api.request.ParkingSpaceChangeApplyReq;
import com.cf.parking.api.request.ParkingSpaceChangeRecordReq;
import com.cf.parking.api.response.ParkingSpaceChangeRecordRsp;
import com.cf.parking.api.response.ParkingSpaceGroupRsp;
import com.cf.parking.facade.bo.ParkingSpaceChangeRecordBO;
import com.cf.parking.facade.bo.ParkingSpaceGroupBO;
import com.cf.parking.facade.dto.ParkingSpaceChangeApplyDTO;
import com.cf.parking.facade.dto.ParkingSpaceChangeRecordDTO;
import com.cf.parking.facade.facade.ParkingSpaceChangeRecordFacade;
import com.cf.parking.facade.facade.UserSpaceFacade;
import com.cf.support.authertication.AdminUserAuthentication;
import com.cf.support.authertication.UserAuthentication;
import com.cf.parking.services.enums.ChangeRecordStateEnum;
import com.cf.parking.services.enums.UserSpaceTypeEnum;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.authertication.token.dto.UserSessionDTO;
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
import java.util.List;

/**
 * 车位互换记录Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Api(tags = "车位互换记录管理模块——摇号系统")
@Slf4j
@RestController
@RequestMapping("/parkingSpace/change")
public class ParkingSpaceChangeRecordController  extends BaseController
{
    @Resource
    private ParkingSpaceChangeRecordFacade parkingSpaceChangeRecordFacade;
    
    
    @Resource
    private UserSpaceFacade userSpaceFacade;
    


    //————————————————PC端————————————————————
    
    /**
     * 查询车位转赠记录列表
     */
    @SuppressWarnings("unchecked")
	@AdminUserAuthentication
    @ApiOperation(value = "查询PC端车位互换记录列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<ParkingSpaceChangeRecordRsp>> pcList(@RequestBody ParkingSpaceChangeRecordReq param)
    {
    	
        log.info("互换记录查询参数：{}",JSON.toJSONString(param));
        //2.参数转换
        ParkingSpaceChangeRecordDTO dto = new ParkingSpaceChangeRecordDTO();
        BeanUtils.copyProperties(param,dto);

        //3.列表查询
        PageResponse<ParkingSpaceChangeRecordBO> result = parkingSpaceChangeRecordFacade.getParkingSpaceChangeRecordList(dto);
        List<ParkingSpaceChangeRecordRsp> transferRecordRsps = BeanConvertorUtils.copyList(result.getList(), ParkingSpaceChangeRecordRsp.class);
        return PageUtils.pageResult(result,transferRecordRsps);
    }
    
    
    //————————————————小程序端————————————————————

    /**
     * 查询车位转赠记录列表
     */
    @SuppressWarnings("unchecked")
	@UserAuthentication
    @ApiOperation(value = "查询车位互换记录列表————小程序", notes = "根据条件分页查询")
    @PostMapping("/applet/list")
    public Result<PageResponse<ParkingSpaceChangeRecordRsp>> appletList(@RequestBody ParkingSpaceChangeRecordReq param)
    {
    	
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();
        param.setUserId(userId);
        log.info("互换记录查询参数：{}",JSON.toJSONString(param));
        //2.参数转换
        ParkingSpaceChangeRecordDTO dto = new ParkingSpaceChangeRecordDTO();
        BeanUtils.copyProperties(param,dto);

        //3.列表查询
        PageResponse<ParkingSpaceChangeRecordBO> result = parkingSpaceChangeRecordFacade.getParkingSpaceChangeRecordList(dto);
        List<ParkingSpaceChangeRecordRsp> transferRecordRsps = BeanConvertorUtils.copyList(result.getList(), ParkingSpaceChangeRecordRsp.class);
        return PageUtils.pageResult(result,transferRecordRsps);
    }

    
    /**
     * 查询用户车位列表
     */
    @UserAuthentication
    @ApiOperation(value = "查询用户的车库", notes = "查询用户的车库")
    @PostMapping("/space/list")
    public Result<List<ParkingSpaceGroupRsp>> spaceList()
    {
    	
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        log.info("查询用户的车库参数：{}",JSON.toJSONString(user));

        List<ParkingSpaceGroupBO> list = userSpaceFacade.getUserSpaceGroupByParkingLot(user.getOpenId(),UserSpaceTypeEnum.LOTTERY.getState());
        List<ParkingSpaceGroupRsp> result = BeanConvertorUtils.copyList(list, ParkingSpaceGroupRsp.class);
        return Result.buildSuccessResult(result);
    }
    
    
    /**
     * 判断是否展示交换车位按钮，true 显示，false不显示
     */
    @UserAuthentication
    @ApiOperation(value = "判断是否展示交换车位按钮", notes = "已申请过或者是有待处理的交换记录时不显示")
    @PostMapping("/check")
    public Result<Boolean> checkShow()
    {
    	
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        log.info("判断是否展示交换车位按钮 用户：{}",JSON.toJSONString(user));
        List<ParkingSpaceGroupBO> list = userSpaceFacade.getUserSpaceGroupByParkingLot(user.getOpenId(),UserSpaceTypeEnum.LOTTERY.getState());
        if(CollectionUtils.isEmpty(list)) {
        	//没有车位不展示
        	return Result.buildSuccessResult(false);
        }
        ParkingSpaceChangeRecordDTO dto = new ParkingSpaceChangeRecordDTO();
        dto.setUserId(user.getUserId());
        dto.setState(ChangeRecordStateEnum.APPLY.getState());
        long count = parkingSpaceChangeRecordFacade.getParkingSpaceChangeRecoudCount(dto);
        return Result.buildSuccessResult(count > 0 ? false : true);
    }
    
    /**
     * 申请互换车位
     */
    @SuppressWarnings("rawtypes")
	@AdminOptLogTitle("申请互换车位")
    @UserAuthentication
    @ApiOperation(value = "申请互换", notes = "申请互换")
    @PostMapping("/apply")
    public Result apply(@RequestBody ParkingSpaceChangeApplyReq param)
    {
    	log.info("查询用户的车库参数：{}",JSON.toJSONString(param));
    	AssertUtil.checkNull(param.getParkingCode(), "车库不能为空");
    	AssertUtil.checkNull(param.getValidStartDate(), "车位有效期起始日不能为空");
    	AssertUtil.checkNull(param.getValidEndDate(), "车位有效期结束日不能为空");
    	AssertUtil.checkNull(param.getAcceptJobNumber(), "交换人工号不能为空");
    	AssertUtil.checkNull(param.getAcceptUserName(), "交换人名称不能为空");
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        param.setUserId(user.getUserId());
        param.setJobNumber(user.getOpenId());
        ParkingSpaceChangeApplyDTO dto = new ParkingSpaceChangeApplyDTO();
        BeanUtils.copyProperties(param, dto);
        parkingSpaceChangeRecordFacade.applyChange(dto);
        return Result.buildSuccessResult();
    }
    
    
    


    /**
     * 撤销/同意/否决
     * @param req
     * @return
     */
    @SuppressWarnings("rawtypes")
	@AdminOptLogTitle("车位交换撤销/同意/否决")
    @UserAuthentication
    @ApiOperation(value = "车位交换撤销/同意/否决", notes = "车位交换撤销/同意/否决")
    @PostMapping("/deal")
    public Result deal( @RequestBody ParkingSpaceChangeApplyReq param){
    	log.info("撤销/同意/否决：{}",JSON.toJSONString(param));
    	AssertUtil.checkNull(param, "参数不存在");
    	AssertUtil.checkNull(param.getId(), "ID不能为空");
    	AssertUtil.checkNull(param.getState(), "状态不能为空");
    	if (ChangeRecordStateEnum.AGREE.getState().equals(param.getState())) {
    		//如果是同意
    		AssertUtil.checkNull(param.getAcceptParkingCode(), "车库不能为空");
        	AssertUtil.checkNull(param.getValidStartDate(), "车位有效期起始日不能为空");
        	AssertUtil.checkNull(param.getValidEndDate(), "车位有效期结束日不能为空");
        	param.setAcceptJobNumber(getUserSessionDTO().getOpenId());
        	param.setAcceptUserId(getUserSessionDTO().getUserId());
    	}
    	ParkingSpaceChangeApplyDTO dto = new ParkingSpaceChangeApplyDTO();
        BeanUtils.copyProperties(param, dto);
    	parkingSpaceChangeRecordFacade.deal(dto);
    	return Result.buildSuccessResult();
    }

}
