package com.cf.parking.dao.mapper;

import org.apache.ibatis.annotations.Param;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.LotteryResultPO;

/**
 * 摇号结果Mapper接口
 * 
 * @author
 * @date 2023-09-05
 */
public interface LotteryResultMapper extends BaseMapper<LotteryResultPO>
{

	/**带状态更新
	 * @param id
	 * @param oldState 原状态
	 * @param newState 新状态
	 * @return
	 */
	int updateByState(@Param("id") Long id, @Param("oldState") String oldState, @Param("newState") String newState);

}
