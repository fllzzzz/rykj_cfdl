package com.cf.parking.dao.mapper;

import org.apache.ibatis.annotations.Param;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.LotteryBatchPO;

/**
 * 摇号批次Mapper接口
 * 
 * @author
 * @date 2023-09-05
 */
public interface LotteryBatchMapper extends BaseMapper<LotteryBatchPO>
{

	/**
	 * 带状态更新
	 * @param id
	 * @param state
	 * @param state2
	 */
	long updateByState(@Param("id") Long id, @Param("oldState") String oldState, @Param("newState") String newState);

}
