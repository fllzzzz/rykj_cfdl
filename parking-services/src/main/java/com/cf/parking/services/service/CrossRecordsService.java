package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.CrossRecordsMapper;
import com.cf.parking.dao.po.CrossRecordsPO;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author lpy
 * @date 2023-03-28 14:05:52
 * @description 过车记录表
 */
@Service
public class CrossRecordsService extends ServiceImpl<CrossRecordsMapper, CrossRecordsPO> implements IService<CrossRecordsPO> {
    @Resource
    private CrossRecordsMapper crossRecordsMapper;

    /**
     * 获取 过车记录
     *
     * @param pageNo
     * @param pageSize
     * @param plateNo
     * @return
     */
    public IPage<CrossRecordsPO> getCrossRecordsByPlateNo(Long pageNo, Long pageSize, String plateNo) {
        Page<CrossRecordsPO> page = new Page<CrossRecordsPO>().setCurrent(pageNo).setSize(pageSize);
        return crossRecordsMapper.selectPage(page, new LambdaQueryWrapper<CrossRecordsPO>()
                .eq(StringUtils.isNotEmpty(plateNo), CrossRecordsPO::getPlateNo, plateNo)
                .orderByDesc(CrossRecordsPO::getCrossTime));
    }

    /**
     * 更新/新增数据
     *
     * @param crossRecordsPOList
     */
    public void saveCrossRecords(List<CrossRecordsPO> crossRecordsPOList) {
        crossRecordsMapper.replaceCrossRecords(crossRecordsPOList);
    }
}

