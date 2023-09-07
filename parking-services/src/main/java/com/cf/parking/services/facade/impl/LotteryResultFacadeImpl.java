package com.cf.parking.services.facade.impl;


import com.alibaba.fastjson.JSON;
import com.cf.parking.dao.mapper.LotteryResultMapper;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.dao.po.LotteryRuleAssignPO;
import com.cf.parking.dao.po.LotteryRuleRoundPO;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.facade.facade.LotteryResultFacade;
import com.cf.parking.services.enums.EnableStateEnum;
import com.cf.parking.services.service.LotteryBatchService;
import com.cf.parking.services.service.LotteryRuleAssignService;
import com.cf.parking.services.service.LotteryRuleRoundService;
import com.cf.parking.services.service.LotteryService;
import com.cf.parking.services.service.ParkingLotService;
import com.cf.parking.services.utils.AssertUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

/**
 * 摇号结果Service业务层处理
 * 
 * @author ruoyi
 * @date 2023-09-05
 */
@Service
@Slf4j
public class LotteryResultFacadeImpl implements LotteryResultFacade
{
    @Resource
    private LotteryResultMapper lotteryResultMapper;

    @Resource
    private LotteryRuleRoundService lotteryRuleRoundService;
    
    @Resource
    private LotteryBatchService lotteryBatchService;
    
    @Resource
    private LotteryService lotteryService;
    
    @Resource
    private LotteryRuleAssignService lotteryRuleAssignService;
    
    @Resource
    private ParkingLotService parkingLotService;
    
    
	@Override
	public void lottery(Long id) {
		
		log.info("开始摇号：{}",id);
		LotteryResultPO lottery = lotteryResultMapper.selectById(id);
		AssertUtil.checkNull(lottery, "数据不存在");
		LotteryRuleRoundPO round = lotteryRuleRoundService.getById(lottery.getRoundId());
		AssertUtil.checkNull(round, "轮次数据不存在");
		AssertUtil.checkTrue(EnableStateEnum.ENABLE.getState().equals(round.getState()),"摇号轮次未启用");
		LotteryBatchPO batch = lotteryBatchService.getById(lottery.getBatchId());
		AssertUtil.checkNull(batch, "批次数据不存在");
		//获取停车场信息
		String parklotCode = round.getParkingLotCode();
		List<String> parkingList = Arrays.asList(parklotCode.split(","));
		List<ParkingLotPO> parkingLotList = parkingLotService.selectParkingLotByCodes(parkingList);
		log.info("获取到摇号停车场信息：{}",JSON.toJSONString(parkingLotList));
		AssertUtil.checkTrue(!CollectionUtils.isEmpty(parkingLotList), "停车场信息不存在，请检查设置");
		parkingLotList = parkingLotList.stream().filter(item -> EnableStateEnum.ENABLE.getState().equals(item.getType())).collect(Collectors.toList());
		AssertUtil.checkTrue(!CollectionUtils.isEmpty(parkingLotList), "停车场的配置为不可参加摇号，请检查设置");
		//获取报名的人员
		parkingLotList.forEach(parkLot -> {
			//获取配置摇号的部门和人员
			
		});
		lotteryService.doLottery(batch,lottery,round);
	}

	
    /**
     * 查询摇号结果
     * 
     * @param id 摇号结果主键
     * @return 摇号结果
     */
//    @Override
//    public LotteryResult selectLotteryResultById(Long id)
//    {
//        return lotteryResultMapper.selectLotteryResultById(id);
//    }

    /**
     * 查询摇号结果列表
     * 
     * @param lotteryResult 摇号结果
     * @return 摇号结果
     */
//    @Override
//    public List<LotteryResult> selectLotteryResultList(LotteryResult lotteryResult)
//    {
//        return lotteryResultMapper.selectLotteryResultList(lotteryResult);
//    }

    /**
     * 新增摇号结果
     * 
     * @param lotteryResult 摇号结果
     * @return 结果
     */
//    @Override
//    public int insertLotteryResult(LotteryResult lotteryResult)
//    {
//        return lotteryResultMapper.insertLotteryResult(lotteryResult);
//    }

    /**
     * 修改摇号结果
     * 
     * @param lotteryResult 摇号结果
     * @return 结果
     */
//    @Override
//    public int updateLotteryResult(LotteryResult lotteryResult)
//    {
//        return lotteryResultMapper.updateLotteryResult(lotteryResult);
//    }

    /**
     * 批量删除摇号结果
     * 
     * @param ids 需要删除的摇号结果主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryResultByIds(Long[] ids)
//    {
//        return lotteryResultMapper.deleteLotteryResultByIds(ids);
//    }

    /**
     * 删除摇号结果信息
     * 
     * @param id 摇号结果主键
     * @return 结果
     */
//    @Override
//    public int deleteLotteryResultById(Long id)
//    {
//        return lotteryResultMapper.deleteLotteryResultById(id);
//    }
}
