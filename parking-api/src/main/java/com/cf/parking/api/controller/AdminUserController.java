package com.cf.parking.api.controller;


import cn.hutool.core.collection.CollectionUtil;
import com.cf.parking.api.annotation.PreventRepeat;
import com.cf.parking.api.request.*;
import com.cf.parking.api.response.*;
import com.cf.parking.facade.bo.AdminTotalPointsBO;
import com.cf.parking.facade.bo.LoginBackBO;
import com.cf.parking.facade.bo.AdminScoreRecordBO;
import com.cf.parking.facade.bo.ScoreRecordBO;
import com.cf.parking.facade.dto.AdminScoreOptDTO;
import com.cf.parking.facade.dto.AdminScoresPageDTO;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.parking.facade.facade.AdminUserFacade;
import com.cf.parking.facade.facade.ScoreRecordFacade;
import com.cf.parking.services.service.ScoreRecordService;
import com.cf.parking.services.service.UserProfileService;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.authertication.AdminAuthenticationServer;
import com.cf.support.authertication.AdminUserAuthentication;
import com.cf.support.authertication.token.dto.AdminSessionDTO;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageRequest;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.ExcelUtiles;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;
import java.util.Date;
import java.util.List;

/**
 * @author weihui
 * @date 2020/10/17
 */

@RestController
@RequestMapping("/admin/user")
@Api(tags = "后台用户管理相关")
public class AdminUserController {

    @Resource
    private AdminUserFacade adminUserFacade;

    @Resource
    private AdminAuthenticationServer adminAuthenticationServer;
    @Resource
    private ScoreRecordFacade scoreRecordFacade;
    @Resource
    private ScoreRecordService scoreRecordService;

    @Resource
    private UserProfileService userProfileService;

    private AdminSessionDTO getAdmin() {
        return adminAuthenticationServer.getCurrentUser();
    }

    @ApiOperation(value = "登录接口", notes = "登录接口")
    @PostMapping(value = "/login")
    public Result<LoginResp> login(@RequestBody LoginReq param) {
        Result<LoginBackBO> facadeResult = adminUserFacade.login(param.getEmplNo(), param.getPassword());
        if (facadeResult.isSuccess() && null != facadeResult.getData()) {
            return Result.buildSuccessResult(BeanConvertorUtils.map(facadeResult.getData(), LoginResp.class));
        }
        return BeanConvertorUtils.map(facadeResult, Result.class);
    }

    @AdminUserAuthentication
    @PostMapping(value = "/logout")
    @ApiOperation(value = "退出登录", notes = "退出登录")
    public Result logout() {
        return adminUserFacade.logout(Long.valueOf(getAdmin().getAdminId()));
    }

    @AdminUserAuthentication
    @ApiOperation(value = "修改密码接口", notes = "修改密码接口")
    @PostMapping(value = "/updatePwd")
    public Result updatePwd(@RequestBody UpdatePwdReq param) {
        return adminUserFacade.updatePwd(getAdmin().getEmplNo(), param.getOldPassword(), param.getNewPassword());
    }

    @ApiOperation(value = "积分分页查询接口", notes = "积分分页查询接口")
    @AdminUserAuthentication
    @PostMapping(value = "/scores")
    public Result<PageResponse<AdminScoreRecordRsp>> getScoresPage(@RequestBody AdminScoresPageReq param) {
        AdminScoresPageDTO adminScoresPageDTO = BeanConvertorUtils.map(param, AdminScoresPageDTO.class);
        PageResponse<AdminScoreRecordBO> scoresPage = adminUserFacade.getScoresPage(adminScoresPageDTO);
        if (scoresPage.getTotal() == 0) {
            return PageUtils.emptyPageResult(scoresPage);
        }
        List<AdminScoreRecordRsp> adminScoreRecordRsp = BeanConvertorUtils.copyList(scoresPage.getList(), AdminScoreRecordRsp.class);
        return PageUtils.pageResult(scoresPage, adminScoreRecordRsp);
    }

    @ApiOperation(value = "积分后台操作接口", notes = "积分后台操作接口")
    @AdminUserAuthentication
    @PreventRepeat(state = 2)
    @PostMapping(value = "/opt")
    public Result scoreOpt(@RequestBody AdminScoreOptReq param) {
        String remark = param.getRemark();
        if (CollectionUtil.isEmpty(param.getUserIdList()) || param.getScore() == null || StringUtils.isEmpty(remark)) {
            throw new BusinessException(BizResultCodeEnum.PARAM_NULL.getMsg());
        }
        //备注字数限制50
        if (remark.length() > 50) {
            throw new BusinessException(BizResultCodeEnum.PARAM_ERROR.getMsg());
        }
        AdminScoreOptDTO adminScoreOptDTO = BeanConvertorUtils.map(param, AdminScoreOptDTO.class);
        adminScoreOptDTO.setAdminUserId(Long.valueOf(getAdmin().getAdminId()));
        return adminUserFacade.scoreOpt(adminScoreOptDTO);
    }

    @ApiOperation(value = "积分查看详情接口", notes = "积分查看详情接口")
    @AdminUserAuthentication
    @PostMapping(value = "/detail")
    public Result<PageResponse> scoreDetail(@RequestBody AdminScoreDetailReq param) {
        Long userId = param.getUserId();
        if (ObjectUtils.isEmpty(userId)) {
            throw new BusinessException(BizResultCodeEnum.PARAM_NULL.getMsg());
        }
        PageRequest pageRequest = BeanConvertorUtils.map(param, PageRequest.class);
        PageResponse<ScoreRecordBO> scoreDetail = scoreRecordFacade.getScoresPage(userId, pageRequest);
        if (ObjectUtils.isEmpty(scoreDetail.getTotal())) {
            return PageUtils.emptyPageResult(scoreDetail);
        }
        List<ScoreRecordRsp> scoreRecordRsp = BeanConvertorUtils.copyList(scoreDetail.getList(), ScoreRecordRsp.class);
        return PageUtils.pageResult(scoreDetail, scoreRecordRsp);
    }

    @ApiOperation(value = "积分记录导出接口", notes = "积分记录导出接口")
    @AdminUserAuthentication
    @PostMapping(value = "/export")
    public void exportScoreList(@RequestBody AdminScoresPageReq param, HttpServletResponse response) {
        AdminScoresPageDTO adminScoresPageDTO = BeanConvertorUtils.map(param, AdminScoresPageDTO.class);
        List<AdminScoreRecordBO> scoresList = userProfileService.getScoresList(adminScoresPageDTO);
        List<ExportScoreListRsp> list = BeanConvertorUtils.copyList(scoresList, ExportScoreListRsp.class);
        ExcelUtiles.exportExcel(list, "积分记录", "积分记录", ExportScoreListRsp.class, "积分记录.xls", response);
    }

    @ApiOperation(value = "总积分查询接口", notes = "总积分查询接口")
    @AdminUserAuthentication
    @PostMapping(value = "/total")
    public Result<AdminTotalPointsRsp> totalPoints() {
        AdminTotalPointsBO totalScores = scoreRecordService.getTotalScores();
        AdminTotalPointsRsp totalPointsRsp = BeanConvertorUtils.map(totalScores, AdminTotalPointsRsp.class);
        return Result.buildSuccessResult(totalPointsRsp);
    }

    @ApiOperation(value = "月积分导出接口", notes = "月积分导出接口")
    @AdminUserAuthentication
    @PostMapping(value = "/export/time")
    public void exportTimeScore(@RequestBody AdminScoreTimeReq param, HttpServletResponse response) {
        Date beginDate = param.getBeginDate();
        Date endDate = param.getEndDate();
        if (ObjectUtils.isEmpty(beginDate) || ObjectUtils.isEmpty(endDate)) {
            throw new BusinessException(BizResultCodeEnum.PARAM_NULL);
        }
        if (beginDate.after(endDate)) {
            throw new BusinessException(BizResultCodeEnum.PARAM_ERROR);
        }
        List<AdminScoreRecordBO> scoresList = scoreRecordFacade.exportTimeScore(beginDate, endDate);
        List<ExportScoreListRsp> list = BeanConvertorUtils.copyList(scoresList, ExportScoreListRsp.class);
        String scoreTitle = DateFormatUtils.format(beginDate, "yyyy/MM/dd") + "-" + DateFormatUtils.format(endDate, "yyyy/MM/dd") + "积分记录";
        ExcelUtiles.exportExcel(list, scoreTitle, scoreTitle, ExportScoreListRsp.class, scoreTitle + ".xls", response);
    }
}
