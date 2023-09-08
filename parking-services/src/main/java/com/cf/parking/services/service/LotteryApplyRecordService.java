package com.cf.parking.services.service;

import java.util.List;
import javax.annotation.Resource;
import org.springframework.stereotype.Service;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryApplyRecordMapper;
import com.cf.parking.dao.po.LotteryApplyRecordPO;



@Service
public class LotteryApplyRecordService extends ServiceImpl<LotteryApplyRecordMapper, LotteryApplyRecordPO> implements IService<LotteryApplyRecordPO>{

	@Resource
	private LotteryApplyRecordMapper applyRecordMapper;
	
	
	
	
	/**
	 * 根据批次号和车库查询申请人员
	 * @param batchId
	 * @param parkingList 
	 * @return
	 */
	public List<LotteryApplyRecordPO> queryLotteryApplyList(Long batchId, List<String> parkingList) {
		return applyRecordMapper.selectList(new LambdaQueryWrapper<LotteryApplyRecordPO>()
				.eq(LotteryApplyRecordPO::getBatchId, batchId)
				.in(LotteryApplyRecordPO::getParkingLotCode, parkingList)
				);
	}

	
}
