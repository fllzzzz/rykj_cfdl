package com.cf.parking.api.controller;

import cn.hutool.core.bean.BeanUtil;
import com.cf.parking.api.annotation.AdminOptLogTitle;
import com.cf.parking.api.request.*;
import com.cf.parking.api.response.*;
import com.cf.parking.facade.bo.*;
import com.cf.parking.facade.dto.*;
import com.cf.parking.facade.facade.*;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.authertication.AdminUserAuthentication;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.ExcelUtiles;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.util.List;

/**
 * @author: lpy
 * @Date: 2023/03/27
 */
@Slf4j
@AdminUserAuthentication
@RestController
@RequestMapping("/parkingSpace")
@Api(tags = "车位管理模块")
public class ParkingSpaceController {
    @Resource
    private BlackListFacade blackListFacade;
    @Resource
    private WhiteListFacade whiteListFacade;
    @Resource
    private ScheduleDataFacade scheduleDataFacade;
    @Autowired
    private CrossRecordsFacade crossRecordsFacade;
    @Resource
    private UserSpaceFacade userSpaceFacade;

    @GetMapping("/blacklist/export")
    @ApiOperation(value = "黑名单导出接口", notes = "黑名单导出接口")
    public void export(HttpServletResponse response) {
        List<BlackListBO> listBOS = blackListFacade.export();
        List<ExportBlackListRsp> list = BeanConvertorUtils.copyList(listBOS, ExportBlackListRsp.class);
        ExcelUtiles.exportExcel(list, "黑名单车辆", "黑名单车辆", ExportBlackListRsp.class, "黑名单车辆.xls", response);
    }

    @PostMapping("/blacklist/bachDel")
    @AdminOptLogTitle("车闸黑名单批量删除接口")
    @ApiOperation(value = "车闸黑名单批量删除接口", notes = "车闸黑名单批量删除接口")
    public Result blackListBatchDel(@RequestBody BlackListBatchDelReq param) {
        if (CollectionUtils.isEmpty(param.getBlackListIds())) {
            return Result.buildErrorResult("列表为空，删除失败");
        }
        return blackListFacade.blackListBatchDel(BeanConvertorUtils.map(param, BlackListBatchDelDTO.class));
    }

    @PostMapping("/blacklist/page")
    @ApiOperation(value = "车闸黑名单分页查询接口", notes = "车闸黑名单分页查询接口")
    public Result blackListPage(@RequestBody @Valid BlackListPageReq param) {
        PageResponse<BlackListPageBO> blackListPage = blackListFacade.page(BeanConvertorUtils.map(param, BlackListPageDTO.class));
        if (blackListPage.getTotal() == 0) {
            return PageUtils.emptyPageResult(blackListPage);
        }
        List<BlackListPageRsp> blackListPageRsps = BeanConvertorUtils.copyList(blackListPage.getList(), BlackListPageRsp.class);
        return PageUtils.pageResult(blackListPage, blackListPageRsps);
    }

    @PostMapping("/settings/save")
    @AdminOptLogTitle("黑名单设置保存接口")
    @ApiOperation(value = "黑名单设置保存接口", notes = "黑名单设置保存接口")
    public Result settingsSave(@RequestBody @Valid BlacklistSettingsReq param) {
        return blackListFacade.settingsSave(BeanConvertorUtils.map(param, BlacklistSettingsDTO.class));
    }

    @PostMapping("/settings/get")
    @ApiOperation(value = "黑名单设置获取接口", notes = "黑名单设置获取接口")
    public Result settingsGet() {
        BlacklistSettingsBO blacklistSettingsBO = blackListFacade.settingsGet();
        return Result.buildSuccessResult(BeanConvertorUtils.map(blacklistSettingsBO, BlacklistSettingsRsp.class));
    }

    @PostMapping("/crossRecords/page")
    @ApiOperation(value = "车辆出入记录", notes = "车辆出入记录")
    public Result<CrossRecordsRsp> getUserSpacePage(@RequestBody CrossRecordsPageReq param) {
        PageResponse<CrossRecordsBO> crossRecordsBOPage = crossRecordsFacade.getPage(param.getPageNo(), param.getPageSize(), param.getPlateNo());
        if (crossRecordsBOPage.getTotal() == 0) {
            return PageUtils.emptyPageResult(crossRecordsBOPage);
        }
        List<CrossRecordsRsp> userSpaceRsps = BeanConvertorUtils.copyList(crossRecordsBOPage.getList(), CrossRecordsRsp.class);
        return PageUtils.pageResult(crossRecordsBOPage, userSpaceRsps);
    }

    @PostMapping(value = "/userSpace/export")
    @ApiOperation(value = "车位导出接口", notes = "车位导出接口")
    public void exportUserSpace(@RequestBody @Valid UserSpacePageReq param, HttpServletResponse response) {
        List<UserSpaceBO> userSpaceList = userSpaceFacade.getUserSpaceList((BeanConvertorUtils.map(param, UserSpacePageDTO.class)));
        List<ExportUserSpaceRsp> list = BeanConvertorUtils.copyList(userSpaceList, ExportUserSpaceRsp.class);
        ExcelUtiles.exportExcel(list, "车位管理记录", "车位管理记录", ExportUserSpaceRsp.class, "车位管理记录.xls", response);
    }

    @PostMapping("/userSpace/page")
    @ApiOperation(value = "车位管理分页接口", notes = "车位管理分页接口")
    public Result getUserSpacePage(@RequestBody UserSpacePageReq param) {
        PageResponse<UserSpaceBO> userSpacePage = userSpaceFacade.getUserSpacePage(BeanConvertorUtils.map(param, UserSpacePageDTO.class));
        if (userSpacePage.getTotal() == 0) {
            return PageUtils.emptyPageResult(userSpacePage);
        }
        List<UserSpaceRsp> userSpaceRsps = BeanConvertorUtils.copyList(userSpacePage.getList(), UserSpaceRsp.class);
        return PageUtils.pageResult(userSpacePage, userSpaceRsps);
    }
    
    @AdminOptLogTitle("黑名单设置保存接口")
    @DeleteMapping("/userSpace/delete")
    @ApiOperation(value = "根据Id删除车位", notes = "根据Id删除车位")
    public Result deleteUserSpace(@RequestBody UserSpacePageReq param) {
    	AssertUtil.checkNull(param.getUserSpaceId(), "车位Id不能为空");
        userSpaceFacade.deleteUserSpace(param.getUserSpaceId());
        return Result.buildSuccessResult();
    }

    @PostMapping("/scheduleData/get")
    @ApiOperation(value = "排班记录查询接口", notes = "排班记录查询接口")
    public Result<ScheduleDateRsp> getScheduleDate() {
        ScheduleDateBO scheduleDate = scheduleDataFacade.getScheduleDate();
        return Result.buildSuccessResult(BeanConvertorUtils.map(scheduleDate, ScheduleDateRsp.class));
    }

    @PostMapping("/whitelist/bachDel")
    @AdminOptLogTitle("白名单批量删除接口")
    @ApiOperation(value = "白名单批量删除接口", notes = "白名单批量删除接口")
    public Result whiteListBatchDel(@RequestBody WhiteListBatchDelReq param) {
        whiteListFacade.whiteListBatchDel(BeanConvertorUtils.map(param, WhiteListBatchDelDTO.class));
        return Result.buildSuccessResult();
    }

    @PostMapping("/whitelist/page")
    @ApiOperation(value = "白名单分页查询接口", notes = "白名单分页查询接口")
    public Result<PageResponse<WhiteListPageRsp>> whiteListPage(@RequestBody WhiteListReq param) {
        WhiteListPageDTO whiteListPageDTO = BeanUtil.toBean(param, WhiteListPageDTO.class);
        PageResponse<WhiteListPageBO> whiteListPage = whiteListFacade.page(whiteListPageDTO);
        if (whiteListPage.getTotal() == 0) {
            return PageUtils.emptyPageResult(whiteListPage);
        }
        List<WhiteListPageRsp> whiteListPageRsps = BeanConvertorUtils.copyList(whiteListPage.getList(), WhiteListPageRsp.class);
        return PageUtils.pageResult(whiteListPage, whiteListPageRsps);
    }

    @PostMapping(value = "/whitelist/add")
    @AdminOptLogTitle("白名单批量新增接口")
    @ApiOperation(value = "白名单批量新增接口", notes = "白名单批量新增接口")
    public Result importUserSpace(@RequestBody WhiteListAddReq whiteListAddReq) {
        whiteListFacade.whiteListSave(BeanConvertorUtils.map(whiteListAddReq, WhiteListAddDTO.class));
        return Result.buildSuccessResult();
    }
}
