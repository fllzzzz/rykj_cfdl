package com.cf.parking.services.facade.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryRuleRoundMapper;
import com.cf.parking.dao.po.LotteryRuleRoundPO;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.facade.bo.LotteryRuleRoundBO;
import com.cf.parking.facade.bo.LotteryRuleRoundBaseBO;
import com.cf.parking.facade.dto.LotteryRuleRoundDTO;
import com.cf.parking.facade.dto.LotteryRuleRoundOptDTO;
import com.cf.parking.facade.facade.LotteryRuleRoundFacade;
import com.cf.parking.services.service.LotteryRuleAssignService;
import com.cf.parking.services.service.ParkingLotService;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;

/**
 * 摇号规则-轮数Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@Service
public class LotteryRuleRoundFacadeImpl implements LotteryRuleRoundFacade
{
    @Autowired
    private LotteryRuleRoundMapper mapper;

    @Resource
    private ParkingLotService parkingLotService;

    @Resource
    private IdWorker idWorker;
    
    @Resource
    private LotteryRuleAssignService lotteryRuleAssignService;
    

    /** 根据摇号轮数id转换成对应的轮数名称并进行拼接
     * roundId：[1,2,3]*/
    public String getNameByRoundId(String roundId){
        String name = null;
        if (StringUtils.isBlank(roundId)){
            return name;
        }

        List<Long> roundIdList = JSON.parseArray(roundId, Long.class);
        if (CollectionUtils.isEmpty(roundIdList)){
            return name;
        }

        List<String> nameList = new ArrayList<>();
        for (Long id : roundIdList) {
            LotteryRuleRoundPO roundPO = mapper.selectById(id);
            if (null != roundPO  && StringUtils.isNotBlank(roundPO.getName())){
                nameList.add(roundPO.getName());
            }
        }
        name = String.join(",", nameList);
        return name;
    }

    /** 根据摇号轮数id转换成对应的数组
     * roundId：[1,2,3]*/
    public Long[] getRoundIdArrByRoundIdStr(String roundId) {
        if (StringUtils.isBlank(roundId)){
            return null;
        }
        List<Long> roundIdList = JSON.parseArray(roundId, Long.class);
        return roundIdList.toArray(new Long[0]);
    }

    /**
     * 查询摇号规则-轮数名称列表
     * @return
     */
    @Override
    public List<LotteryRuleRoundBaseBO> selectBaseList() {
        List<LotteryRuleRoundPO> poList = mapper.selectBaseList();
        List<LotteryRuleRoundBaseBO> baseBOList = BeanConvertorUtils.copyList(poList, LotteryRuleRoundBaseBO.class);
        return baseBOList;
    }

    /**
     * 查询摇号规则-轮数列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<LotteryRuleRoundBO> getLotteryRuleRoundList(LotteryRuleRoundDTO dto) {
        Page<LotteryRuleRoundPO> page = PageUtils.toPage(dto);

        Page<LotteryRuleRoundPO> poPage = mapper.selectPage(page, new LambdaQueryWrapper<LotteryRuleRoundPO>()
                .eq(ObjectUtils.isNotEmpty(dto.getId()), LotteryRuleRoundPO::getId, dto.getId())
                .like(StringUtils.isNotBlank(dto.getName()), LotteryRuleRoundPO::getName, dto.getName())
                .eq(StringUtils.isNotBlank(dto.getState()), LotteryRuleRoundPO::getState, dto.getState()));

        List<LotteryRuleRoundBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryRuleRoundBO.class);
        //设置停车场名称和车位数量
        boList.stream().forEach(this::setParkingLotNameAndAmount);

        return PageUtils.toResponseList(page,boList);
    }

    private void setParkingLotNameAndAmount(LotteryRuleRoundBO bo) {
        String parkingLotCode = bo.getParkingLotCode();
        ParkingLotPO parkingLotPO = parkingLotService.selectParkingLotByCode(parkingLotCode);
        if (parkingLotPO != null) {
        	bo.setParkingLotName(parkingLotPO.getRegion());
        	bo.setParkingLotAmount(parkingLotPO.getAmount());
        }
    }

    /**
     * 新增摇号规则-轮数
     * @param dto
     * @return
     */
    @Override
    public Integer add(LotteryRuleRoundOptDTO dto) {
        LotteryRuleRoundPO po = new LotteryRuleRoundPO();
        BeanUtils.copyProperties(dto,po);
        po.setId(idWorker.nextId());
        po.setCreateTm(new Date());
        po.setUpdateTm(new Date());
        int result = mapper.insert(po);
        log.info("添加摇号规则-轮数成功  ——  {}",po);
        return result;
    }

    /**
     * 修改摇号规则-轮数
     * @param dto
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer update(LotteryRuleRoundOptDTO dto) {
        LotteryRuleRoundPO po = new LotteryRuleRoundPO();
        BeanUtils.copyProperties(dto,po);
        po.setUpdateTm(new Date());
        int result = mapper.updateById(po);
        log.info("修改摇号规则-轮数成功  ——  {}",po);
        lotteryRuleAssignService.updateByRoundId(po.getId());
        return result;
    }

    /**
     * 删除摇号规则-轮数
     * @param id
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer deleteById(Long id) {
            int result = mapper.deleteById(id);
            lotteryRuleAssignService.deleteByRoundId(id);
            log.info("摇号规则-轮数删除成功，id：{}",id);
            return result;
    }


    /**
     * 根据id查询轮数
     * @param roundId
     * @return
     */
    public LotteryRuleRoundPO getLotteryRuleRoundByRoundId(Long roundId) {
        return mapper.selectById(roundId);
    }
}
