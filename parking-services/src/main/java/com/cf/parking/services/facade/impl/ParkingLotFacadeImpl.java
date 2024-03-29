package com.cf.parking.services.facade.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.ParkingLotMapper;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.facade.bo.*;
import com.cf.parking.facade.dto.*;
import com.cf.parking.facade.facade.ParkingLotFacade;
import com.cf.parking.services.integration.GatewayHikvisionFeign;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.dao.DataIntegrityViolationException;
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

    @Resource
    private GatewayHikvisionFeign gatewayHikvisionFeign;

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
            setParentNameByBolist(boList);
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
                setParentNameByBolist(boList);
                return PageUtils.toResponseList(page,boList);
            }
        }
        return PageUtils.toResponseList(page,new ArrayList<>());
    }

    private void setParentNameByBolist(List<ParkingLotBO> boList) {
        for (ParkingLotBO parkingLotBO : boList) {
            setParentNameByBo(parkingLotBO);
        }
    }

    private void setParentNameByBo(ParkingLotBO parkingLotBO) {
        parkingLotBO.setParentName(mapper.selectById(parkingLotBO.getParentId()).getRegion());
    }


    private List<ParkingLotBO> getParkingLotBOList(List<ParkingLotPO> list) {
        List<ParkingLotBO> boList = BeanConvertorUtils.copyList(list, ParkingLotBO.class);
        for (ParkingLotBO parkingLotBO : boList) {
            parkingLotBO.setChildren(getParkingLotChildren(parkingLotBO));
            setParentNameByBo(parkingLotBO);
        }
        return boList;
    }

    private List<ParkingLotBO> getParkingLotChildren(ParkingLotBO parkingLotBO) {
        List<ParkingLotPO> childPOList = mapper.selectList(new LambdaQueryWrapper<ParkingLotPO>().eq(ParkingLotPO::getParentId, parkingLotBO.getId()));
        List<ParkingLotBO> childBoList = BeanConvertorUtils.copyList(childPOList, ParkingLotBO.class);
        List<ParkingLotBO> result = childBoList.stream().map(bo -> {
            bo.setChildren(getParkingLotChildren(bo));
            setParentNameByBo(bo);
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
        //1.po对象生成
        ParkingLotPO po = new ParkingLotPO();
        BeanUtils.copyProperties(dto,po);
        po.setId(idWorker.nextId());
        po.setCreateTm(new Date());
        po.setUpdateTm(new Date());

        //2.图片信息设置
        setImageInfo(dto.getFiles(), po);

        try{
            //3.新增
            int result = mapper.insertWithImageInfo(po);
            log.info("新增停车场成功  ——  {}",po);
            return result;
        } catch (DataIntegrityViolationException e){
            log.error("新增停车场重复：{}，失败原因：{}",po,e);
            throw new BusinessException("停车场编码已存在，请修改后重试！");
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
        setImageInfo(dto.getFiles(), po);
        try{
            int result = mapper.updateWithImageInfo(po);
            log.info("修改停车场成功  ——  {}",po);
            return result;
        } catch (DataIntegrityViolationException e){
            log.error("修改停车场重复：{}，失败原因：{}",po,e);
            throw new BusinessException("园区编码已存在，请修改后重试！");
        } 
    }

    private void setImageInfo(List<ParkingLotImageDTO> images, ParkingLotPO po) {
        if (CollectionUtils.isNotEmpty(images)){
            String imageInfo = JSON.toJSONString(images);
            po.setImageInfo(imageInfo);
        }
    }

    /**
     * 删除停车场
     * 注：删除时需要删除子停车场
     * @param id
     * @return
     */
    @Override
    public Integer deleteById(Long id) {
            //1.根据id查询
            List<Long> ids = new ArrayList<>();
            ids.add(id);
            List<Long> poList = mapper.selectList(new LambdaQueryWrapper<ParkingLotPO>().eq(ParkingLotPO::getParentId, id)).stream().map(ParkingLotPO::getId).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(poList)){
                ids.addAll(poList);
                while (CollectionUtils.isNotEmpty(poList)){
                    List<Long> innerIds = new ArrayList<>();
                    for (Long childId : poList) {
                        List<Long> childList = mapper.selectList(new LambdaQueryWrapper<ParkingLotPO>().eq(ParkingLotPO::getParentId, childId)).stream().map(ParkingLotPO::getId).collect(Collectors.toList());
                        innerIds.addAll(childList);
                    }
                    if (CollectionUtils.isNotEmpty(innerIds)){
                        ids.addAll(innerIds);
                        poList = innerIds;
                    }else {
                        break;
                    }
                }
            }
            int result = mapper.deleteBatchIds(ids);
            log.info("停车场删除成功，id：{}",id);
            return result;
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
        String regionCode = JSON.toJSONString(entranceList);

        //2.生成对象
        ParkingLotPO po = new ParkingLotPO();
        po.setId(idWorker.nextId()).setCreateTm(new Date()).setUpdateTm(new Date()).setParentId(0L).setRegion(dto.getName()).setRegionCode(regionCode);

        try{
            int result = mapper.insert(po);
            log.info("新增停车场园区成功  ——  {}",po);
            return result;
        } catch (DataIntegrityViolationException e){
            log.error("新增停车场园区入口重复：{}，失败原因：{}",po,e);
            throw new BusinessException("园区编码已存在，请修改后重试！");
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
        String regionCode = JSON.toJSONString(entranceList);

        //2.生成对象
        ParkingLotPO po = new ParkingLotPO().setId(dto.getId()).setRegion(dto.getName()).setRegionCode(regionCode).setUpdateTm(new Date());

        try{
            int result = mapper.updateById(po);
            log.info("修改停车场园区成功  ——  {}",po);
            return result;
        }catch (DataIntegrityViolationException e){
            log.error("修改停车场园区入口重复：{}，失败原因：{}",po,e);
            throw new BusinessException("入口重复，请修改后重试！");
        } 
    }

    /**
     * 查询停车场树形列表
     * @return
     */
    @Override
    public List<ParkingLotTreeBO> getParkingLotTreeList() {
        List<ParkingLotPO> poList = mapper.selectList(new LambdaQueryWrapper<ParkingLotPO>().eq(ParkingLotPO::getParentId, 0));
        List<ParkingLotTreeBO> boList = BeanConvertorUtils.copyList(poList, ParkingLotTreeBO.class);
        for (ParkingLotTreeBO treeBO : boList) {
            treeBO.setChildren(getParkingLotTreeChildren(treeBO));
        }
        return boList;
    }

    /**
     * 查询所有非园区的停车场id-region列表
     * @return
     */
    @Override
    public List<ParkingLotBO> getParkingLotIdRegionList() {
        LambdaQueryWrapper<ParkingLotPO> queryWrapper = new LambdaQueryWrapper<ParkingLotPO>().select(ParkingLotPO::getId, ParkingLotPO::getRegion).ne(ParkingLotPO::getParentId,0);
        List<ParkingLotPO> poList = mapper.selectList(queryWrapper);

        return BeanConvertorUtils.copyList(poList,ParkingLotBO.class);
    }

    /**
     * 根据id查询停车场信息
     * @return
     */
    @Override
    public ParkingLotImageBO getParkingLotInfoById(Long id) {
        //根据id查询包含图片信息的停车场对象信息
        ParkingLotPO po = mapper.selectParkingLotPOWithImageInfoById(id);

        ParkingLotImageBO bo = new ParkingLotImageBO();
        BeanUtils.copyProperties(po, bo);
        //将字符串转为图片信息对象
        List<ParkingLotImageInfoBO> parkingLotImageInfoBOS = JSON.parseObject(po.getImageInfo(), new TypeReference<List<ParkingLotImageInfoBO>>() {});
        bo.setFiles(parkingLotImageInfoBOS);
        bo.setParentName(mapper.selectById(po.getParentId()).getRegion());
        return bo;
    }


    private List<ParkingLotTreeBO> getParkingLotTreeChildren(ParkingLotTreeBO treeBO) {
        List<ParkingLotPO> childPOList = mapper.selectList(new LambdaQueryWrapper<ParkingLotPO>().eq(ParkingLotPO::getParentId, treeBO.getId()));
        List<ParkingLotTreeBO> childBoList = BeanConvertorUtils.copyList(childPOList, ParkingLotTreeBO.class);
        List<ParkingLotTreeBO> result = childBoList.stream().map(bo -> {
            bo.setChildren(getParkingLotTreeChildren(bo));
            return bo;
        }).collect(Collectors.toList());
        return result;
    }

    /**
     * 查询园区列表
     * @return
     */
    @Override
    public List<ParkingLotAreaBO> getAreaList(String region) {
        List<ParkingLotPO> poList = mapper.selectList(new LambdaQueryWrapper<ParkingLotPO>()
                .eq(ParkingLotPO::getParentId, 0)
                .like(StringUtils.isNotBlank(region),ParkingLotPO::getRegion,region));
        List<ParkingLotAreaBO> boList = poList.stream().map(po -> {
            ParkingLotAreaBO bo = new ParkingLotAreaBO().setId(po.getId()).setName(po.getRegion());
            if (StringUtils.isNotBlank(po.getRegionCode())) {
                List<ParkingLotAreaEntranceBO> entranceBOList = JSON.parseArray(po.getRegionCode(),ParkingLotAreaEntranceBO.class );
                bo.setEntranceList(entranceBOList);
            }
            return bo;
        }).collect(Collectors.toList());
        return boList;
    }

	@Override
	public List<ParkingLotImageBO> getAllParkingLot() {
		List<ParkingLotImageBO> result = new ArrayList<>();
		List<ParkingLotPO> list = mapper.getAllParkingLot();
        list.forEach(po -> {
        	 ParkingLotImageBO bo = new ParkingLotImageBO();
             BeanUtils.copyProperties(po, bo);
             //将字符串转为图片信息对象
             List<ParkingLotImageInfoBO> parkingLotImageInfoBOS = JSON.parseObject(po.getImageInfo(), new TypeReference<List<ParkingLotImageInfoBO>>() {});
             bo.setFiles(parkingLotImageInfoBOS);
             result.add(bo);
        });
        
		return result;
	}

}
