package com.cf.parking.services.facade.impl;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.LotteryBatchMapper;
import com.cf.parking.dao.po.*;
import com.cf.parking.facade.bo.LotteryBatchBO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.facade.dto.LotteryBatchDTO;
import com.cf.parking.facade.dto.LotteryBatchOptDTO;
import com.cf.parking.facade.facade.LotteryBatchFacade;
import com.cf.parking.services.service.LotteryResultService;
import com.cf.parking.services.service.ParkingLotService;
import com.cf.parking.services.utils.AssertUtil;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.formula.functions.T;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import javax.annotation.Resource;

/**
 * 摇号批次Service业务层处理
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@Service
public class LotteryBatchFacadeImpl implements LotteryBatchFacade
{
    @Resource
    private LotteryBatchMapper mapper;

    @Resource
    private LotteryRuleRoundFacadeImpl lotteryRuleRoundFacade;

    @Resource
    private LotteryResultService lotteryResultService;

    @Resource
    private ParkingLotService parkingLotService;

    @Resource
    private IdWorker idWorker;

    /**
     * 查询摇号批次列表
     * @param dto
     * @return
     */
    @Override
    public PageResponse<LotteryBatchBO> getLotteryBatchList(LotteryBatchDTO dto) {
        Page<LotteryBatchPO> page = PageUtils.toPage(dto);

        LambdaQueryWrapper<LotteryBatchPO> queryWrapper = new LambdaQueryWrapper<LotteryBatchPO>()
                .le(!ObjectUtils.isEmpty(dto.getEndDate()), LotteryBatchPO::getBatchNum, DateUtil.format(dto.getEndDate(), "yyyy-MM-dd 23:59:59"))
                .ge(!ObjectUtils.isEmpty(dto.getStartDate()) , LotteryBatchPO::getBatchNum, DateUtil.format(dto.getEndDate(), "yyyy-MM-dd 00:00:00"))
                .like(!ObjectUtils.isEmpty(dto.getRoundId()) , LotteryBatchPO::getRoundId, dto.getRoundId())
                .eq(StringUtils.isNotEmpty(dto.getState()), LotteryBatchPO::getState, dto.getState())
                .orderByDesc(LotteryBatchPO::getUpdateTm);

        Page<LotteryBatchPO> poPage = mapper.selectPage(page, queryWrapper);
        List<LotteryBatchBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryBatchBO.class);
        //生成摇号规则名称、摇号轮数数组
        boList.forEach(bo -> {
            String lotteryRule = lotteryRuleRoundFacade.getNameByRoundId(bo.getRoundId());
            bo.setLotteryRule(lotteryRule);
            Long[] roundIdArr = lotteryRuleRoundFacade.getRoundIdArrByRoundIdStr(bo.getRoundId());
            bo.setRoundIdArr(roundIdArr);
        });
        return PageUtils.toResponseList(page,boList);
    }


    /**
     * 新增摇号批次
     * @param dto
     * @return
     */
    @Override
    public Integer add(LotteryBatchOptDTO dto) {
        //1.参数复制
        LotteryBatchPO po = new LotteryBatchPO();
        BeanUtils.copyProperties(dto,po);

        //2.参数设置
        po.setId(idWorker.nextId());
        //TODO:枚举
        po.setState("0");
        po.setCreateTm(new Date());
        po.setUpdateTm(new Date());
        //2.1轮数设置（将数组转为字符串）
        AssertUtil.checkNull(dto.getRoundIdArr(),"请选择摇号轮数");
        Long[] roundIdArr = dto.getRoundIdArr();
        String roundId = Arrays.toString(roundIdArr).replaceAll("\\s+","");
        po.setRoundId(roundId);

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
        //轮数设置（将数组转为字符串）
        AssertUtil.checkNull(dto.getRoundIdArr(),"请选择摇号轮数");
        Long[] roundIdArr = dto.getRoundIdArr();
        String roundId = Arrays.toString(roundIdArr).replaceAll("\\s+","");
        po.setRoundId(roundId);
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
     * 已结束的摇号批次进行结果查看
     * @param dto
     * @return
     */
    @Override
    public PageResponse<LotteryResultDetailBO> viewResult(LotteryBatchDTO dto) {
        Page<LotteryResultDetailPO> page = PageUtils.toPage(dto);
        PageResponse<LotteryResultDetailBO> boPageResponse = lotteryResultService.viewResult(page,dto.getId(),dto.getRoundId());
        return boPageResponse;
    }

    /**
     * 根据摇号轮数查询车位数量
     * @param roundIdArr
     * @return
     */
    @Override
    public Long getParkingAmountByRound(Long[] roundIdArr) {
        Long parkingAmount = 0L;
        for (Long roundId : roundIdArr) {
            //1.根据轮数查询停车场编码
            LotteryRuleRoundPO round = lotteryRuleRoundFacade.getLotteryRuleRoundByRoundId(roundId);
            //2.根据停车场编码查询车位数量
            ParkingLotPO parkingLot = parkingLotService.selectParkingLotByCode(round.getParkingLotCode());
            parkingAmount += parkingLot.getAmount();
        }
        return parkingAmount;
    }

}
