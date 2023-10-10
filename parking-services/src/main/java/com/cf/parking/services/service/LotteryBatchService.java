package com.cf.parking.services.service;

import cn.hutool.core.date.DateField;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.cf.parking.services.enums.LotteryBatchStateEnum;
import com.cf.parking.services.utils.AssertUtil;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryBatchMapper;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.LotteryRuleRoundPO;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.facade.dto.LotteryBatchOptDTO;
import com.cf.parking.facade.facade.LotteryBatchFacade;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import javax.annotation.Resource;


@Slf4j
@Service
public class LotteryBatchService extends ServiceImpl<LotteryBatchMapper, LotteryBatchPO> implements IService<LotteryBatchPO>{

    @Resource
    private LotteryBatchMapper lotteryBatchMapper;

    @Resource
    private LotteryRuleRoundService lotteryRuleRoundService;
    
    @Resource
    private ParkingLotService parkingLotService;
    
    @Resource
    private LotteryBatchFacade lotteryBatchFacade;
    
    /**
     * 将对应批次状态修改为已结束
     * @param batchId
     */
    public Integer endByBatchId(Long batchId) {
        LotteryBatchPO po = new LotteryBatchPO();
        po.setId(batchId);
        po.setState(LotteryBatchStateEnum.HAVE_END.getState());
        return lotteryBatchMapper.updateById(po);
    }

	/**
	 * 判断是否存在包含下月的摇号批次，true：存在，false:不存在
	 * @param 开始日期
	 * @param 结束日期
	 * @return
	 */
	public boolean queryNextMonthBatchExist(String startDate, String endDate) {
		long count = lotteryBatchMapper.selectCount(new LambdaQueryWrapper<LotteryBatchPO>()
					.le(LotteryBatchPO::getValidStartDate, startDate)
					.ge(LotteryBatchPO::getValidEndDate, endDate)
				);
		return count > 0;
	}

	/**
	 * 自动创建摇号批次
	 */
	@Transactional(rollbackFor = Exception.class)
	public void autoCreateBatch() {

		LotteryBatchOptDTO dto = new LotteryBatchOptDTO();
		//期号
		dto.setBatchNum(new Date());
		//查询轮号
		LotteryRuleRoundPO round = lotteryRuleRoundService.queryDefaultRound();
		AssertUtil.checkNull(round, "不存在有效轮次");
		dto.setRoundIdArr(new Long[] {round.getId()});
		ParkingLotPO parkingLot = parkingLotService.selectParkingLotByCode(round.getParkingLotCode());
		AssertUtil.checkNull(parkingLot, "车库不存在"); 
		dto.setParkingAmount(parkingLot.getAmount());
		dto.setApplyStartTime(new DateTime(DateUtil.tomorrow()));
		dto.setApplyEndTime(new DateTime(DateUtils.addDays(new Date(), 5)));
		dto.setValidStartDate(DateUtil.beginOfMonth( DateUtil.nextMonth()));
		dto.setValidStartDate(DateUtil.endOfMonth( DateUtil.nextMonth()));
		log.info("自动创建批次任务参数：{}",JSON.toJSONString(dto));
		lotteryBatchFacade.add(dto);
	}

	/**
     * 查询最新一期摇号批次信息（期号最大的、状态为已通知或已结束的）
     * @return
     */
    public LotteryBatchPO getNotifiedLatestBatchInfo() {
		List<LotteryBatchPO> batchPOList = lotteryBatchMapper.selectList(new LambdaQueryWrapper<LotteryBatchPO>()
				.ne(LotteryBatchPO::getState, LotteryBatchStateEnum.NEED_NOTIFY.getState())
				.orderByDesc(LotteryBatchPO::getBatchNum));
		if (CollectionUtils.isNotEmpty(batchPOList)){
			return batchPOList.get(0);
		}
		return null;
    }


    /**
     * 判断当前时间是否处于报名时间内
	 * 注：申请截止日期在数据库内存的是00:00:00，但实际在截止日期当天23:59:59仍应可以报名
     * @param applyStartTime
     * @param applyEndTime
     * @return
     */
    public boolean judgeWhetherInApplyTime(Date applyStartTime, Date applyEndTime) {
        Date now = new Date();
		Calendar endTime = Calendar.getInstance();
		endTime.setTime(applyEndTime);
		endTime.set(Calendar.HOUR_OF_DAY, 23);
		endTime.set(Calendar.MINUTE, 59);
		endTime.set(Calendar.SECOND, 59);
		return now.before(endTime.getTime()) && now.after(applyStartTime);
    }
}
