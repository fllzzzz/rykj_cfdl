package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.BlackListMapper;
import com.cf.parking.dao.po.BlackListPO;
import com.cf.parking.facade.enums.IsAsyncEnum;
import com.cf.parking.facade.enums.IsDeleteEnum;
import com.cf.parking.services.integration.GatewayHikvisionFeign;
import com.cf.support.exception.BusinessException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author lpy
 * @date 2023-03-27 09:43:43
 * @description 黑名单记录表
 */
@Service
@Slf4j
public class BlackListService extends ServiceImpl<BlackListMapper, BlackListPO> implements IService<BlackListPO> {
    @Resource
    private BlackListService blackListService;
    @Resource
    private BlackListMapper blackListMapper;
    @Resource
    private GatewayHikvisionFeign gatewayHikvisionFeign;


    /**
     * 逻辑删除黑名单车辆
     *
     * @param blackIds ids
     */
    public void logicDelBlackListCar(List<Long> blackIds) {
        boolean update = blackListService.update(new LambdaUpdateWrapper<BlackListPO>()
                .eq(BlackListPO::getIsDelete, IsDeleteEnum.FALSE.getCode())
                .in(BlackListPO::getBlackListId, blackIds)
                .set(BlackListPO::getIsDelete, IsDeleteEnum.TRUE.getCode()));
        if (!update) {
            throw new BusinessException("删除失败，请刷新页面重试");
        }
    }

    /**
     * 根据id获取未删除的黑名单车
     *
     * @param blackIds ids
     * @return List
     */
    public List<BlackListPO> getBlackListByIds(List<Long> blackIds) {
        return blackListService.list(new LambdaQueryWrapper<BlackListPO>()
                .eq(BlackListPO::getIsDelete, IsDeleteEnum.FALSE.getCode())
                .in(BlackListPO::getBlackListId, blackIds));
    }

    /**
     * 精确查询获取分页
     *
     * @param page      page
     * @param jobNumber jobNumber
     * @param plateNo   plateNo
     * @return Ipage
     */
    public IPage queryPage(Page page, String jobNumber, String plateNo) {
        return blackListMapper.selectPage(page, new LambdaQueryWrapper<BlackListPO>()
                .eq(BlackListPO::getIsDelete, IsDeleteEnum.FALSE.getCode())
                .eq(StringUtils.isNotEmpty(jobNumber), BlackListPO::getJobNumber, jobNumber)
                .eq(StringUtils.isNotEmpty(plateNo), BlackListPO::getPlateNo, plateNo));
    }


    /**
     * 获取未删除的黑名单车
     *
     * @return 车牌号
     */
    public List<String> getBlackList() {
        return blackListService.list(new LambdaQueryWrapper<BlackListPO>()
                        .eq(BlackListPO::getIsDelete, IsDeleteEnum.FALSE.getCode())
                ).stream().map(BlackListPO::getPlateNo).collect(Collectors.toList());
    }


    /**
     * 更新alarmSysCode和isAsync
     *
     * @param blackListPO
     */
    public void updateBlackListById(BlackListPO blackListPO) {
        boolean isUpdate = this.update(new LambdaUpdateWrapper<BlackListPO>()
                .eq(BlackListPO::getBlackListId, blackListPO.getBlackListId())
                .set(ObjectUtils.isNotEmpty(blackListPO.getIsAsync()), BlackListPO::getIsAsync, blackListPO.getIsAsync())
                .set(StringUtils.isNotEmpty(blackListPO.getAlarmSyscode()), BlackListPO::getAlarmSyscode, blackListPO.getAlarmSyscode())
        );
        if (!isUpdate) {
            throw new BusinessException("更新失败，请稍候重试");
        }
    }

    /**
     * 添加黑名单 ：逐个添加 ,事务传播等级：嵌套开启新事务
     *
     * @param blackListPO
     */
    @Transactional(propagation = Propagation.NESTED, rollbackFor = Exception.class)
    public void addBlackList(BlackListPO blackListPO) {
        // 执行sql
        blackListPO.setIsAsync(IsAsyncEnum.TRUE.getCode());
        this.updateBlackListById(blackListPO);

        // 调接口
//        BlackListAdditionDTO additionDTO = BeanConvertorUtils.map(blackListPO, BlackListAdditionDTO.class);
//        log.info("调用黑名单添加接口参数：{}", JSON.toJSONString(additionDTO));
//        // TODO:先注掉黑名单加入逻辑
//        HikvisionResult<BlackListAlarmSyscodeDTO> listAddition = gatewayHikvisionFeign.blackListAddition(FeignUrlConstant.BLACK_LIST_ADD_URL, additionDTO);
//        log.info("调用黑名单添加接口返回：{}", JSON.toJSONString(listAddition));
//
////         更新alarmSysCode
//        String alarmSyscode = listAddition.getData().getAlarmSyscode();
//        log.info("{} 设置alarmSysCode：{}", JSON.toJSONString(blackListPO), alarmSyscode);
//        blackListPO.setAlarmSyscode(alarmSyscode);
//        this.updateBlackListById(blackListPO);
    }

    /**
     * 根据车牌号获取未删除的数据，防止重复添加
     *
     * @param plateNos plateNos
     * @return list
     */
    public List<BlackListPO> queryByPlateNo(List<String> plateNos) {
        return this.list(new LambdaQueryWrapper<BlackListPO>()
                .in(BlackListPO::getPlateNo, plateNos)
                .eq(BlackListPO::getIsDelete, IsDeleteEnum.FALSE.getCode()));
    }
}

