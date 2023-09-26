package com.cf.parking.services.facade.impl;

import java.util.Date;
import java.util.List;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryRuleAssignMapper;
import com.cf.parking.dao.po.LotteryRuleAssignPO;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.facade.bo.LotteryRuleAssignBO;
import com.cf.parking.facade.dto.LotteryRuleAssignDTO;
import com.cf.parking.facade.dto.LotteryRuleAssignOptDTO;
import com.cf.parking.facade.facade.LotteryRuleAssignFacade;
import com.cf.parking.services.service.ParkingLotService;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * 摇号规则-停车场分配Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@Service
public class LotteryRuleAssignFacadeImpl implements LotteryRuleAssignFacade
{
    @Resource
    private LotteryRuleAssignMapper mapper;

    @Resource
    private IdWorker idWorker;

    @Resource
    private ParkingLotService parkingLotService;

    /**
     * 查询摇号规则-停车场分配列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<LotteryRuleAssignBO> getLotteryRuleAssignList(LotteryRuleAssignDTO dto) {
        Page<LotteryRuleAssignPO> page = PageUtils.toPage(dto);

        Page<LotteryRuleAssignPO> poPage = mapper.selectPage(page, new LambdaQueryWrapper<LotteryRuleAssignPO>()
                .eq(StringUtils.isNotBlank(dto.getType()), LotteryRuleAssignPO::getType, dto.getType())
                .eq(ObjectUtils.isNotEmpty(dto.getRoundId()),LotteryRuleAssignPO::getRoundId,dto.getRoundId())
                .like(StringUtils.isNotBlank(dto.getName()), LotteryRuleAssignPO::getName, dto.getName())
                .eq(StringUtils.isNotBlank(dto.getParkingLotCode()), LotteryRuleAssignPO::getParkingLotCode, dto.getParkingLotCode())
                .eq(StringUtils.isNotBlank(dto.getState()), LotteryRuleAssignPO::getState, dto.getState())
                .orderByAsc(LotteryRuleAssignPO::getCreateTm));

        List<LotteryRuleAssignBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryRuleAssignBO.class);

        //根据停车场code查询停车场名称返回前端
        boList.forEach(this::setParkingLotName);
        return PageUtils.toResponseList(page,boList);
    }

    private void setParkingLotName(LotteryRuleAssignBO lotteryRuleAssignBO) {
    	ParkingLotPO parking = parkingLotService.selectParkingLotByCode(lotteryRuleAssignBO.getParkingLotCode());
        if (parking != null) {
        	lotteryRuleAssignBO.setParkingLotName(parking.getRegion());
        }
    }

    /**
     * 新增摇号规则-停车场分配
     * @param dto
     * @return
     */
    @Override
    public Integer add(LotteryRuleAssignOptDTO dto) {
        LotteryRuleAssignPO po = new LotteryRuleAssignPO();
        BeanUtils.copyProperties(dto,po);
        po.setId(idWorker.nextId());
        po.setCreateTm(new Date());
        po.setUpdateTm(new Date());
        try{
            int result = mapper.insert(po);
            log.info("停车场分配新增成功  ——  {}",po);
            return result;
        }catch (Exception e){
            log.error("停车场分配新增失败：{}，失败原因：{}",po,e);
            return 0;
        }
    }

    /**
     * 修改摇号规则-停车场分配
     * @param dto
     * @return
     */
    @Override
    public Integer update(LotteryRuleAssignOptDTO dto) {
        LotteryRuleAssignPO po = new LotteryRuleAssignPO();
        BeanUtils.copyProperties(dto,po);
        po.setUpdateTm(new Date());
        try{
            int result = mapper.updateById(po);
            log.info("停车场分配修改成功  ——  {}",po);
            return result;
        }catch (Exception e){
            log.error("停车场分配修改失败：{}，失败原因：{}",po,e);
            return 0;
        }
    }

    /**
     * 删除摇号规则-停车场分配
     * @param id
     * @return
     */
    @Override
    public Integer deleteById(Long id) {
        try{
            int result = mapper.deleteById(id);
            log.info("停车场分配删除成功，id：{}",id);
            return result;
        }catch (Exception e){
            log.error("停车场分配删除失败，id：{}，失败原因：{}",id,e);
            return 0;
        }
    }


}
