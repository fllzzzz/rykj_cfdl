package com.cf.parking.services.service;

import java.util.List;
import javax.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
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
	 * @param parkingLot 
	 * @return
	 */
	public List<LotteryApplyRecordPO> queryLotteryApplyList(Long batchId, String parkingLot) {
		return applyRecordMapper.selectList(new LambdaQueryWrapper<LotteryApplyRecordPO>()
				.eq(LotteryApplyRecordPO::getBatchId, batchId)
				.eq(StringUtils.hasText(parkingLot),LotteryApplyRecordPO::getParkingLotCode, parkingLot)
				);
	}




	/**
	 * 
	 * @param openIdList 工号
	 * @param result 结果
	 */
	public void updateResultByJobNum(List<String> openIdList, String parkCode) {
		if (CollectionUtils.isEmpty(openIdList)) {
			return;
		}
		LotteryApplyRecordPO record = new LotteryApplyRecordPO().setResult("1").setParkingLotCode(parkCode);
		applyRecordMapper.update(record, new LambdaUpdateWrapper<LotteryApplyRecordPO>().in(LotteryApplyRecordPO::getJobNumber, openIdList));
	}




	/**
	 * 根据批次ID查询报名人数
	 * @param batchId
	 * @return
	 */
	public Long queryApplyCountByBatchId(Long batchId) {
		return applyRecordMapper.selectCount(new LambdaQueryWrapper<LotteryApplyRecordPO>()
					.eq(LotteryApplyRecordPO::getBatchId, batchId)
				);
	}

	
}
