package com.cf.parking.services.facade.impl;

import cn.hutool.core.date.DateUtil;
import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.cf.parking.dao.po.BlackListPO;
import com.cf.parking.dao.po.BlacklistSettingsPO;
import com.cf.parking.facade.bo.BlackListBO;
import com.cf.parking.facade.bo.BlackListPageBO;
import com.cf.parking.facade.bo.BlacklistSettingsBO;
import com.cf.parking.facade.dto.*;
import com.cf.parking.facade.enums.IsDeleteEnum;
import com.cf.parking.facade.facade.BlackListFacade;
import com.cf.parking.services.integration.GatewayHikvisionFeign;
import com.cf.parking.services.service.BlackListService;
import com.cf.parking.services.service.BlacklistSettingsService;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.DingAlarmUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author: lpy
 * @Date: 2023/03/27
 */
@Service
@Slf4j
public class BlackListFacadeImpl implements BlackListFacade {
    public static final Map<Integer, String> blackListMap = new HashMap<>();

    static {
        blackListMap.put(IsDeleteEnum.FALSE.getCode(), IsDeleteEnum.FALSE.getMsg());
        blackListMap.put(IsDeleteEnum.TRUE.getCode(), IsDeleteEnum.TRUE.getMsg());
    }

    @Resource
    private BlackListService blackListService;

    @Resource
    private GatewayHikvisionFeign gatewayHikvisionFeign;
    @Resource
    private BlacklistSettingsService blacklistSettingsService;
    @Resource
    private IdWorker idWorker;

    @Override
    public List<BlackListBO> export() {
        List<BlackListPO> blackListPOS = blackListService.list(new LambdaQueryWrapper<BlackListPO>());
        return blackListPOS.stream().map(o -> {
            BlackListBO blackListBO = BeanConvertorUtils.map(o, BlackListBO.class);
            blackListBO.setIsDelete(blackListMap.get(o.getIsDelete()));
            blackListBO.setCreateTm(DateUtil.format(o.getCreateTm(), "yyyy-MM-dd HH:mm:ss"));
            return blackListBO;
        }).collect(Collectors.toList());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result blackListBatchDel(BlackListBatchDelDTO param) {
        List<BlackListPO> blackListPOS = blackListService.getBlackListByIds(param.getBlackListIds());
        String sysCodes = blackListPOS.stream()
                .map(BlackListPO::getAlarmSyscode)
                .distinct()
                .collect(Collectors.joining(","));

        // 逻辑删除数据
        blackListService.logicDelBlackListCar(param.getBlackListIds());

        // 调海康接口
        BlackListBatchDelAlarmSyscodeDTO syscodeDTO = new BlackListBatchDelAlarmSyscodeDTO();
        syscodeDTO.setAlarmSyscodes(sysCodes);
        log.info("批量删除黑名单：{}", JSON.toJSONString(syscodeDTO));
        try {
            // TODO:暂时先注调这块逻辑，上线打开
            log.info("delete_black_list:{}", JSON.toJSONString(syscodeDTO));
//            gatewayHikvisionFeign.blackListDeletion(FeignUrlConstant.BLACK_LIST_DELETION_URL, syscodeDTO);
        } catch (Exception e) {
            log.info(e.getMessage());
            // 如果海康那边重复删除会报这个错Invalid Resource.(Detail: target alarmCar not existed)
            if (!e.getMessage().contains("target alarmCar not existed")) {
                throw e;
            }
        }
        return Result.buildSuccessResult();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result blackListAddition(List<BlackListBatchAdditionDTO> list) {
        ArrayList<String> plateNos = new ArrayList<>(list.size());
        // 批量插入黑名单表
        list.forEach(o -> {
            // 如果没有id的话添加id
            if (ObjectUtils.isEmpty(o.getBlackListId())) {
                o.setBlackListId(idWorker.nextId());
            }
            plateNos.add(o.getPlateNo());
        });
        List<BlackListPO> originBlackList = blackListService.queryByPlateNo(plateNos);
        Map<String, BlackListPO> plateNoMap = originBlackList.stream().collect(Collectors.toMap(BlackListPO::getPlateNo, item -> item));

        List<BlackListPO> blackListPOS = BeanConvertorUtils.copyList(list, BlackListPO.class);

        blackListPOS = blackListPOS.stream().filter(o -> {
            if (ObjectUtils.isNotEmpty(plateNoMap.get(o.getPlateNo()))) {
                return false;
            }
            return true;
        }).collect(Collectors.toList());
        blackListPOS.addAll(originBlackList);

        blackListService.saveOrUpdateBatch(blackListPOS);

        blackListPOS.forEach(o -> {
            try {
                blackListService.addBlackList(o);
            } catch (Exception e) {
                String content = "添加黑名单失败：info:" + JSON.toJSONString(o);
                log.error(content);
                DingAlarmUtils.alarmException(content);
            }
        });
        return Result.buildSuccessResult();
    }

    @Override
    public PageResponse<BlackListPageBO> page(BlackListPageDTO param) {
        IPage blackListPage = blackListService.queryPage(PageUtils.toPage(param), param.getJobNumber(), param.getPlateNo());
        if (blackListPage.getTotal() == 0) {
            return PageUtils.emptyResponseList(blackListPage);
        }

        List<BlackListPO> records = blackListPage.getRecords();
        List<BlackListPageBO> pageBOS = records.stream().map(o -> {
            BlackListPageBO blackListPageBO = BeanConvertorUtils.map(o, BlackListPageBO.class);
            blackListPageBO.setCreateTm(DateUtil.format(o.getCreateTm(), "yyyy-MM-dd HH:mm:ss"));
            return blackListPageBO;
        }).collect(Collectors.toList());

        return PageUtils.toResponseList(blackListPage, pageBOS);
    }

    @Override
    public Result settingsSave(BlacklistSettingsDTO param) {
        param.setBlacklistSettingsId(1L);
        log.info("黑名单设置保存：{}", JSON.toJSONString(param));
        blacklistSettingsService.saveOrUpdate(BeanConvertorUtils.map(param, BlacklistSettingsPO.class));
        return Result.buildSuccessResult();
    }

    @Override
    public BlacklistSettingsBO settingsGet() {
        // 获取设置
        BlacklistSettingsPO settingsServiceById = blacklistSettingsService.getById(1);
        return BeanConvertorUtils.map(settingsServiceById, BlacklistSettingsBO.class);
    }
}
