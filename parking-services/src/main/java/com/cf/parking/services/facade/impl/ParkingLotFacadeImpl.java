package com.cf.parking.services.facade.impl;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.ParkingLotMapper;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.facade.bo.ParkingLotAreaBO;
import com.cf.parking.facade.bo.ParkingLotAreaEntranceBO;
import com.cf.parking.facade.bo.ParkingLotBO;
import com.cf.parking.facade.dto.ParkingLotAreaEntranceOptDTO;
import com.cf.parking.facade.dto.ParkingLotAreaOptDTO;
import com.cf.parking.facade.dto.ParkingLotDTO;
import com.cf.parking.facade.dto.ParkingLotOptDTO;
import com.cf.parking.facade.facade.ParkingLotFacade;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
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
     * 查询停车场列表（层级结构）
     * @param dto
     * @return
     */
    @Override
    public PageResponse<ParkingLotBO> getParkingLotList(ParkingLotDTO dto) {
        Page<ParkingLotPO> page = PageUtils.toPage(dto);

        //1.如果parentId不为空，查询直接下级；然后对直接下级设置子记录
        if (ObjectUtils.isNotEmpty(dto.getParentId())){
            //1.1直接下级
            Page<ParkingLotPO> poPage = mapper.selectPage(page, new LambdaQueryWrapper<ParkingLotPO>()
                    .eq(ObjectUtils.isNotEmpty(dto.getId()), ParkingLotPO::getId, dto.getId())
                    .eq(ParkingLotPO::getParentId, dto.getParentId())
                    .like(StringUtils.isNotBlank(dto.getRegion()), ParkingLotPO::getRegion, dto.getRegion())
                    .eq(StringUtils.isNotBlank(dto.getType()), ParkingLotPO::getType, dto.getType())
                    .orderByAsc(ParkingLotPO::getCreateTm));

            List<ParkingLotBO> boList = getParkingLotBOList(poPage.getRecords());
            return PageUtils.toResponseList(page,boList);
        }else {
            //2.如果parentId为空，先查询所有第一级（园区）的子记录，然后查询子记录
            List<ParkingLotPO> areaPOList = mapper.selectList(new LambdaQueryWrapper<ParkingLotPO>().eq(ParkingLotPO::getParentId, 0));
            if (CollectionUtils.isNotEmpty(areaPOList)){
                List<Long> parentIdList = areaPOList.stream().map(ParkingLotPO::getId).collect(Collectors.toList());
                Page<ParkingLotPO> poPage = mapper.selectPage(page, new LambdaQueryWrapper<ParkingLotPO>()
                        .eq(ObjectUtils.isNotEmpty(dto.getId()), ParkingLotPO::getId, dto.getId())
                        .in(ParkingLotPO::getParentId, parentIdList)
                        .like(StringUtils.isNotBlank(dto.getRegion()), ParkingLotPO::getRegion, dto.getRegion())
                        .eq(StringUtils.isNotBlank(dto.getType()), ParkingLotPO::getType, dto.getType())
                        .orderByAsc(ParkingLotPO::getCreateTm));

                List<ParkingLotBO> boList = getParkingLotBOList(poPage.getRecords());
                return PageUtils.toResponseList(page,boList);
            }
        }
        return PageUtils.toResponseList(page,new ArrayList<>());
    }

    private List<ParkingLotBO> getParkingLotBOList(List<ParkingLotPO> list) {
        List<ParkingLotBO> boList = BeanConvertorUtils.copyList(list, ParkingLotBO.class);
        for (ParkingLotBO parkingLotBO : boList) {
            parkingLotBO.setChildren(getChildren(parkingLotBO));
        }
        return boList;
    }

    private List<ParkingLotBO> getChildren(ParkingLotBO parkingLotBO) {
        List<ParkingLotPO> childPOList = mapper.selectList(new LambdaQueryWrapper<ParkingLotPO>().eq(ParkingLotPO::getParentId, parkingLotBO.getId()));
        List<ParkingLotBO> childBoList = BeanConvertorUtils.copyList(childPOList, ParkingLotBO.class);
        List<ParkingLotBO> result = childBoList.stream().map(bo -> {
            bo.setChildren(getChildren(bo));
            return bo;
        }).collect(Collectors.toList());
        return result;
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

    /**
     * 新增园区
     * @param dto
     * @return
     */
    @Override
    public Integer addArea(ParkingLotAreaOptDTO dto) {
        //1.根据传入的入口信息转成区域编号
        List<ParkingLotAreaEntranceOptDTO> entranceList = dto.getEntranceList();
        Gson gson = new Gson();
        String regionCode = gson.toJson(entranceList);

        //2.生成对象
        ParkingLotPO po = new ParkingLotPO();
        po.setId(idWorker.nextId()).setCreateTm(new Date()).setUpdateTm(new Date()).setParentId(0L).setRegion(dto.getName()).setRegionCode(regionCode);

        try{
            int result = mapper.insert(po);
            log.info("新增停车场园区成功  ——  {}",po);
            return result;
        }catch (Exception e){
            log.error("新增停车场园区失败：{}，失败原因：{}",po,e);
            return 0;
        }
    }

    /**
     * 修改园区
     * @param dto
     * @return
     */
    @Override
    public Integer updateArea(ParkingLotAreaOptDTO dto) {
        //1.根据传入的入口信息转成区域编号
        List<ParkingLotAreaEntranceOptDTO> entranceList = dto.getEntranceList();
        Gson gson = new Gson();
        String regionCode = gson.toJson(entranceList);

        //2.生成对象
        ParkingLotPO po = new ParkingLotPO().setId(dto.getId()).setRegion(dto.getName()).setRegionCode(regionCode).setUpdateTm(new Date());

        try{
            int result = mapper.updateById(po);
            log.info("修改停车场园区成功  ——  {}",po);
            return result;
        }catch (Exception e){
            log.error("修改停车场园区失败：{}，失败原因：{}",po,e);
            return 0;
        }
    }

    /**
     * 查询园区列表
     * @return
     */
    @Override
    public List<ParkingLotAreaBO> getAreaList() {
        List<ParkingLotPO> poList = mapper.selectList(new LambdaQueryWrapper<ParkingLotPO>()
                .eq(ParkingLotPO::getParentId, 0));
        List<ParkingLotAreaBO> boList = poList.stream().map(po -> {
            ParkingLotAreaBO bo = new ParkingLotAreaBO().setId(po.getId()).setName(po.getRegion());
            if (StringUtils.isNotBlank(po.getRegionCode())) {
                Gson gson = new Gson();
                Type type = new TypeToken<List<ParkingLotAreaEntranceBO>>() {
                }.getType();
                List<ParkingLotAreaEntranceBO> entranceBOList = gson.fromJson(po.getRegionCode(), type);
                bo.setEntranceList(entranceBOList);
            }
            return bo;
        }).collect(Collectors.toList());
        return boList;
    }

}
