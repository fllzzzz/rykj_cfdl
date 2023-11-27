package com.cf.parking.api.controller;

import com.alibaba.fastjson.JSON;
import com.cf.parking.api.annotation.AdminOptLogTitle;
import com.cf.parking.api.request.UserVerifyOptReq;
import com.cf.parking.api.request.UserVerifyReq;
import com.cf.parking.api.response.ExportUserVerifyRsp;
import com.cf.parking.api.response.UserProfileRsp;
import com.cf.parking.api.response.UserVerifyRsp;
import com.cf.parking.dao.po.LotteryBlackListPO;
import com.cf.parking.facade.bo.UserProfileBO;
import com.cf.parking.facade.bo.UserVerifyBO;
import com.cf.parking.facade.dto.UserVerifyDTO;
import com.cf.parking.facade.dto.UserVerifyOptDTO;
import com.cf.parking.facade.facade.UserVerifyFacade;
import com.cf.parking.services.enums.PictureInfoEnum;
import com.cf.parking.services.service.LotteryBlackListService;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.support.authertication.AdminUserAuthentication;
import com.cf.support.authertication.UserAuthentication;
import com.cf.support.authertication.token.dto.UserSessionDTO;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.ExcelUtiles;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import net.coobird.thumbnailator.Thumbnails;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.util.Base64;
import java.util.Collections;
import java.util.List;

/**
 * 车辆审核
 * @author
 * @date 2023/9/7
 */
@Api(tags = "车辆审核模块——摇号系统")
@Slf4j
@RestController
@RequestMapping("/user/verify")
public class UserVerifyController extends BaseController {

    @Resource
    private UserVerifyFacade userVerifyFacade;

    @Resource
    private LotteryBlackListService lotteryBlackListService;




    //————————————————PC端————————————————————
    /**
     * 查询车辆审核列表
     */
    @AdminUserAuthentication
    @ApiOperation(value = "查询车辆审核列表", notes = "根据条件分页查询")
    @PostMapping("/list")
    public Result<PageResponse<UserVerifyRsp>> list(@RequestBody UserVerifyReq param)
    {
        UserVerifyDTO dto = new UserVerifyDTO();
        BeanUtils.copyProperties(param,dto);

        PageResponse<UserVerifyBO> result = userVerifyFacade.getPageUserVerifyList(dto);
        List<UserVerifyRsp> verifyRsps = BeanConvertorUtils.copyList(result.getList(), UserVerifyRsp.class);
        return Result.buildSuccessResult(new PageResponse(verifyRsps,result.getPageNo(),result.getTotal(),result.getPageSize()));
    }

    /**
     * 获取单条审核车辆审核详细信息
     */
    @AdminUserAuthentication
    @ApiOperation(value = "获取个人车辆审核详细信息——小程序", notes = "选择停车场后，自动调用查询")
    @PostMapping("/infoById")
    public Result<UserVerifyRsp> getInfoById(@RequestBody UserVerifyReq param)
    {
    	log.info("获取个人车辆审核详细信息——小程序:{}", JSON.toJSONString(param));
        //1.参数校验
        AssertUtil.checkNull(param.getId(),"请选择要审核的记录！");

        //2.查询
        UserVerifyBO bo = userVerifyFacade.getUserVerifyInfoById(param.getId());
        return Result.buildSuccessResult(BeanConvertorUtils.map(bo, UserVerifyRsp.class));
    }


    /**
     * 审核车辆
     */
    @AdminOptLogTitle("进行车辆审核")
    @AdminUserAuthentication
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
    @AdminOptLogTitle("批量审核车辆")
    @AdminUserAuthentication
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

    /**
     * 车辆审核信息批量导出
     */
    @AdminUserAuthentication
    @ApiOperation(value = "车辆审核信息批量导出", notes = "车辆审核信息批量导出")
    @PostMapping("/batchExport")
    public void batchExport(@RequestBody UserVerifyOptReq param, HttpServletResponse response)
    {
        //1.参数转换
        UserVerifyDTO dto = new UserVerifyDTO();
        BeanUtils.copyProperties(param,dto);

        //2.查询符合条件的要导出的记录
        List<UserVerifyBO> boList = userVerifyFacade.getAllUserVerifyList(dto);

        //3.对记录内的base64字符串转换成图片并进行导出
        if (CollectionUtils.isEmpty(boList)){
        	List<ExportUserVerifyRsp> exportUserVerifyRsps = Collections.emptyList();
            ExcelUtiles.exportExcel(exportUserVerifyRsps, "车辆审核记录", "车辆审核记录", ExportUserVerifyRsp.class, "车辆审核记录.xlsx", response);
        }else {
            userVerifyFacade.batchExport(boList,response);
        }
    }



    //————————————————小程序端————————————————————

    /**
     * 获取个人车牌号列表
     */
    @UserAuthentication
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
    @UserAuthentication
    @ApiOperation(value = "获取个人车辆审核详细信息——小程序", notes = "选择停车场后，自动调用查询")
    @PostMapping("/info")
    public Result<UserVerifyRsp> getInfo(@RequestBody UserVerifyReq param)
    {
    	log.info("获取个人车辆审核详细信息——小程序:{}", JSON.toJSONString(param));
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();

        //2.参数校验
        AssertUtil.checkNull(param.getPlateNo(),"请选择车牌号！");

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
     * 单张文件上传(转为base64返回给前端)
     */
    @ApiOperation(value = "单张文件上传——小程序", notes = "单张文件上传")
    @PostMapping("/imageUpload")
    public Result<String> imageUpload(MultipartFile image) throws IOException {
    	log.info("开始上传文件");
        //1.参数校验
        if (null == image){
        	log.info("未选择图片上传");
            return Result.buildErrorResult("未接收到图片！");
        }
        if (!PictureInfoEnum.CONTENT_TYPE_JPG.getInfo().equals(image.getContentType()) &&
                !PictureInfoEnum.CONTENT_TYPE_PNG.getInfo().equals(image.getContentType())){
            throw new BusinessException("请上传JPG或PNG类型的图片");
        }

        //2.图片压缩
        
       byte[] compressedBytes = imageCompress(image);
       log.info("压缩后文件大小：{}kb",compressedBytes.length/1024);

        //2.图片转为base64
        String  base64ImgStr = getBase64ImgStr(compressedBytes);
        return Result.buildSuccessResult(base64ImgStr);
    }

    //图片压缩
    private byte[] imageCompress(MultipartFile image) throws IOException {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        //scale:0-1,表示占原图片的长宽比；outputQuality：0-1，表示压缩质量，1最好，0最差；outputFormat：jpg，将图片压缩后的格式设置成jpg，因为png格式是一种无损的图片格式，无法正确压缩
        Thumbnails.of(image.getInputStream()).scale(1).outputQuality(0.2f).outputFormat("jpg").toOutputStream(outputStream);
        return outputStream.toByteArray();
    }

    /**
     * 新增车辆审核
     */
    @UserAuthentication
    @ApiOperation(value = "新增车辆审核——小程序", notes = "移动端个人中心模块点击车辆录入")
    @PostMapping("/add")
    public Result add(@RequestBody UserVerifyOptReq param)
    {
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();

        //2.申请资格校验
        //2.1是否为黑名单判断
        LotteryBlackListPO lotteryBlackListPO = lotteryBlackListService.queryBlackUserInfo(userId);
        if (null != lotteryBlackListPO){
            return Result.buildErrorResult("暂无权限，请联系管理员！");
        }

        //2.2判断车辆数量是否大于2
        List<String> platNoList = userVerifyFacade.getPlatNoListByUserId(userId);
        if (CollectionUtils.isNotEmpty(platNoList) && platNoList.size() >= 2){
            return Result.buildErrorResult("最多只能录入2辆车！");
        }


        //3.参数校验
        paramVerify(param);
        //3.2判断车牌号是否重复
        if (userVerifyFacade.judgePlateNoRepeat(param.getPlateNo())){
            return Result.buildErrorResult("车牌号已存在，请联系管理员！");
        }


        //4.参数转换
        UserVerifyOptDTO  dto = BeanConvertorUtils.map(param, UserVerifyOptDTO.class);
        dto.setUserId(userId);

        //5.新增
        Integer result = userVerifyFacade.add(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("提交审核失败，请重试！");
    }


    private String getBase64ImgStr(byte[] compressedBytes) throws IOException {
        return PictureInfoEnum.BASE64_JPG_PRE.getInfo() + Base64.getEncoder().encodeToString(compressedBytes);
    }

    private void paramVerify(UserVerifyOptReq param) {
        AssertUtil.checkNull(param.getPlateNo(),"请输入车牌号！");
        AssertUtil.checkNull(param.getVehicleImg(),"请上传车辆照片！");
        AssertUtil.checkNull(param.getDrivingLicenseImg(),"请上传驾驶证照片！");
        AssertUtil.checkNull(param.getDrivingPermitImg(),"请上传行驶证照片！");
    }


    /**
     * 修改个人车辆审核详细信息
     */
    @AdminOptLogTitle("小程序用户修改车辆信息")
    @UserAuthentication
    @ApiOperation(value = "修改个人车辆审核详细信息——小程序", notes = "修改个人车辆审核详细信息")
    @PostMapping("/update")
    public Result update(@RequestBody UserVerifyOptReq param)
    {
        //1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();

        //2.1是否为黑名单判断
        LotteryBlackListPO lotteryBlackListPO = lotteryBlackListService.queryBlackUserInfo(userId);
        if (null != lotteryBlackListPO){
            return Result.buildErrorResult("暂无权限，请联系管理员！");
        }

        //3.参数校验
        AssertUtil.checkNull(param.getId(),"请选择要修改的车辆信息！");
        paramVerify(param);

        //4.参数转换
        UserVerifyOptDTO  dto = BeanConvertorUtils.map(param, UserVerifyOptDTO.class);


        //5.修改
        Integer result = userVerifyFacade.update(dto);
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("提交审核失败，请重试！");
    }

    /**
     * 删除个人车辆信息
     */
    @AdminOptLogTitle("小程序用户删除车辆信息")
    @UserAuthentication
    @ApiOperation(value = "删除个人车辆信息——小程序", notes = "删除个人车辆信息")
    @PostMapping("/delete")
    public Result<String> delete(@RequestBody UserVerifyReq param)
    {
    	log.info("小程序用户删除车辆信息:{}", JSON.toJSONString(param));
        AssertUtil.checkNull(param.getId(),"请选择要删除的记录！");
        Integer result = userVerifyFacade.deleteById(param.getId());
        return result > 0 ?  Result.buildSuccessResult() : Result.buildErrorResult("删除失败，请重试！");
    }


    /**
     * 小程序端根据车牌号查询车主信息
     */
    @UserAuthentication
    @ApiOperation(value = "根据车牌号查询车主信息——小程序端")
    @PostMapping("/infoByPlateNo")
    public Result<UserProfileRsp> getInfoByPlateNo(@RequestBody UserVerifyReq param)
    {
        log.info("根据车牌号查询车主详细信息——小程序:{}", JSON.toJSONString(param));

        //1.参数校验
        AssertUtil.checkNull(param.getPlateNo(),"请选择车牌号！");

        //2.查询
        UserProfileBO bo = userVerifyFacade.getInfoByPlateNo(param.getPlateNo());
        if (null == bo){
            return Result.buildErrorResult("车主信息未录入摇号系统，请联系管理员查询！");
        }
        return Result.buildSuccessResult(BeanConvertorUtils.map(bo,UserProfileRsp.class));
    }

    
    /**
     * 判断用户是否有审核通过的车辆
     */
    @UserAuthentication
    @ApiOperation(value = "判断用户是否有审核通过的车辆")
    @GetMapping("/checkAuditCar")
    public Result<Boolean> checkAuditCar()
    {
    	//1.获取当前登录用户的信息
        UserSessionDTO user = getUserSessionDTO();
        Long userId = user.getUserId();
        //2.查询
        long num = userVerifyFacade.queryAuditedCarCount(userId);
        return Result.buildSuccessResult(num > 0);
    }
    
}
