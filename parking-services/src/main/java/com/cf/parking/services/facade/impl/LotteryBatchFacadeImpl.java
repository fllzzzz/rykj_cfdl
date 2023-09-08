package com.cf.parking.services.facade.impl;

import java.util.Date;
import java.util.List;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryBatchMapper;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.UserVerifyPO;
import com.cf.parking.facade.bo.LotteryBatchBO;
import com.cf.parking.facade.dto.LotteryBatchDTO;
import com.cf.parking.facade.dto.LotteryBatchOptDTO;
import com.cf.parking.facade.facade.LotteryBatchFacade;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import javax.annotation.Resource;

/**
 * 摇号批次Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Slf4j
@Service
public class LotteryBatchFacadeImpl implements LotteryBatchFacade
{
    @Autowired
    private LotteryBatchMapper mapper;

    @Resource
    private LotteryRuleRoundFacadeImpl lotteryRuleRoundFacade;

    /**
     * 查询摇号批次列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<LotteryBatchBO> getLotteryBatchList(LotteryBatchDTO dto) {
        Page<LotteryBatchPO> page = PageUtils.toPage(dto);

        LambdaQueryWrapper<LotteryBatchPO> queryWrapper = new LambdaQueryWrapper<LotteryBatchPO>()
                .le(!ObjectUtils.isEmpty(dto.getEndDate()), LotteryBatchPO::getBatchNum, dto.getEndDate())
                .ge(!ObjectUtils.isEmpty(dto.getStartDate()) , LotteryBatchPO::getBatchNum, dto.getStartDate())
                .like(!ObjectUtils.isEmpty(dto.getRoundId()) , LotteryBatchPO::getRoundId, dto.getRoundId())
                .eq(StringUtils.isNotEmpty(dto.getState()), LotteryBatchPO::getState, dto.getState())
                .orderByDesc(LotteryBatchPO::getUpdateTm);

        Page<LotteryBatchPO> poPage = mapper.selectPage(page, queryWrapper);
        List<LotteryBatchBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryBatchBO.class);
        //生成摇号规则轮数
        boList.forEach(bo -> {
            String lotteryRule = lotteryRuleRoundFacade.getNameByRoundId(bo.getRoundId());
            bo.setLotteryRule(lotteryRule);
        });
        return PageUtils.toResponseList(page,boList);
    }

    /**
     * 获取摇号批次详细信息
     * @param dto
     * @return
     */
    @Override
    public LotteryBatchBO getInfo(LotteryBatchDTO dto) {
        LotteryBatchPO po = mapper.selectById(dto.getId());
        LotteryBatchBO bo = new LotteryBatchBO();
        BeanUtils.copyProperties(po,bo);

        String lotteryRule = lotteryRuleRoundFacade.getNameByRoundId(bo.getRoundId());
        bo.setLotteryRule(lotteryRule);
        return bo;
    }

    /**
     * 新增摇号批次
     * @param dto
     * @return
     */
    @Override
    public Integer add(LotteryBatchOptDTO dto) {
        LotteryBatchPO po = new LotteryBatchPO();
        BeanUtils.copyProperties(dto,po);
        po.setCreateTm(new Date());
        po.setUpdateTm(new Date());
        try{
            int result = mapper.insert(po);
            log.info("新增摇号批次成功  ——  {}",po);
            return result;
        }catch (Exception e){
            log.error("新增摇号批次失败：{}，失败原因：{}",po,e);
            return 0;
        }
    }

    /**
     * 修改摇号批次
     * @param dto
     * @return
     */
    @Override
    public Integer update(LotteryBatchOptDTO dto) {
        LotteryBatchPO po = new LotteryBatchPO();
        BeanUtils.copyProperties(dto,po);
        po.setUpdateTm(new Date());
        try{
            int result = mapper.updateById(po);
            log.info("修改摇号批次成功  ——  {}",po);
            return result;
        }catch (Exception e){
            log.error("修改摇号批次失败：{}，失败原因：{}",po,e);
            return 0;
        }
    }

    /**
     * 删除摇号批次
     * @param id
     * @return
     */
    @Override
    public Integer deleteById(Long id) {
        try{
            int result = mapper.deleteById(id);
            log.info("删除摇号批次成功，id：{}",id);
            return result;
        }catch (Exception e){
            log.error("删除摇号批次失败：{}，失败原因：{}",id,e);
            return 0;
        }
    }

    /**
     * 查询摇号批次
     * 
     * @param id 摇号批次主键
     * @return 摇号批次
     */
//    @Override
//    public LotteryBatch selectLotteryBatchById(Long id)
//    {
//        return lotteryBatchMapper.selectLotteryBatchById(id);
//    }

    /**
     * 查询摇号批次列表
     * 
     * @param lotteryBatch 摇号批次
     * @return 摇号批次
     */
//    @Override
//    public List<LotteryBatch> selectLotteryBatchList(LotteryBatch lotteryBatch)
//    {
//        return lotteryBatchMapper.selectLotteryBatchList(lotteryBatch);
//    }

    /**
     * 新增摇号批次
     * 
     * @param lotteryBatch 摇号批次
     * @return 结果
     */
//    @Override
//    public int insertLotteryBatch(LotteryBatch lotteryBatch)
//    {
//        return lotteryBatchMapper.insertLotteryBatch(lotteryBatch);
//    }

    /**
     * 修改摇号批次
     * 
     * @param lotteryBatch 摇号批次
     * @return 结果
     */
//    @Override
//    public int updateLotteryBatch(LotteryBatch lotteryBatch)
//    {
//        return lotteryBatchMapper.updateLotteryBatch(lotteryBatch);
//    }

    /**
     * 批量删除摇号批次
     * 
     * @param ids 需要删除的摇号批次主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryBatchByIds(Long[] ids)
//    {
//        return lotteryBatchMapper.deleteLotteryBatchByIds(ids);
//    }

    /**
     * 删除摇号批次信息
     * 
     * @param id 摇号批次主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryBatchById(Long id)
//    {
//        return lotteryBatchMapper.deleteLotteryBatchById(id);
//    }
}
