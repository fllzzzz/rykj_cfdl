package com.cf.parking.services.facade.impl;

import java.util.Date;
import java.util.List;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.ParkingLotMapper;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.facade.bo.ParkingLotBO;
import com.cf.parking.facade.dto.ParkingLotDTO;
import com.cf.parking.facade.dto.ParkingLotOptDTO;
import com.cf.parking.facade.facade.ParkingLotFacade;
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
 * 停车场Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@Service
public class ParkingLotFacadeImpl implements ParkingLotFacade
{
    @Resource
    private ParkingLotMapper mapper;

    @Resource
    private IdWorker idWorker;

    /**
     * 查询停车场列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<ParkingLotBO> getParkingLotList(ParkingLotDTO dto) {
        Page<ParkingLotPO> page = PageUtils.toPage(dto);

        Page<ParkingLotPO> poPage = mapper.selectPage(page, new LambdaQueryWrapper<ParkingLotPO>()
                .eq(ObjectUtils.isNotEmpty(dto.getId()), ParkingLotPO::getId, dto.getId())
                .like(StringUtils.isNotBlank(dto.getRegion()), ParkingLotPO::getRegion, dto.getRegion())
                .eq(StringUtils.isNotBlank(dto.getType()), ParkingLotPO::getType, dto.getType())
                .orderByAsc(ParkingLotPO::getCreateTm));

        List<ParkingLotBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), ParkingLotBO.class);
        return PageUtils.toResponseList(page,boList);
    }

    /**
     * 新增停车场
     * @param dto
     * @return
     */
    @Override
    public Integer add(ParkingLotOptDTO dto) {
        ParkingLotPO po = new ParkingLotPO();
        BeanUtils.copyProperties(dto,po);
        po.setId(idWorker.nextId());
        po.setCreateTm(new Date());
        po.setUpdateTm(new Date());
        try{
            int result = mapper.insert(po);
            log.info("新增停车场成功  ——  {}",po);
            return result;
        }catch (Exception e){
            log.error("新增停车场失败：{}，失败原因：{}",po,e);
            return 0;
        }
    }

    /**
     * 修改停车场
     * @param dto
     * @return
     */
    @Override
    public Integer update(ParkingLotOptDTO dto) {
        ParkingLotPO po = new ParkingLotPO();
        BeanUtils.copyProperties(dto,po);
        po.setUpdateTm(new Date());
        try{
            int result = mapper.updateById(po);
            log.info("修改停车场成功  ——  {}",po);
            return result;
        }catch (Exception e){
            log.error("修改停车场失败：{}，失败原因：{}",po,e);
            return 0;
        }
    }

    /**
     * 删除停车场
     * @param id
     * @return
     */
    @Override
    public Integer deleteById(Long id) {
        try{
            int result = mapper.deleteById(id);
            log.info("停车场删除成功，id：{}",id);
            return result;
        }catch (Exception e){
            log.error("停车场删除失败，id：{}，失败原因：{}",id,e);
            return 0;
        }
    }

}
