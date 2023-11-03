package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.UserSpacePO;

import java.util.List;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * @author lpy
 * @date 2023-03-27 16:56:05
 * @description 用户车位表
 */
@Mapper
public interface UserSpaceMapper extends BaseMapper<UserSpacePO> {
    /**
     * 清除库中数据，清空id
     */
    void deleteAll();

	/**
	 * 查询员工的车位并根据车库、有效期进行分组
	 * @param outJobNum
	 * @return
	 */
	List<UserSpacePO> querySpaceGroupByExpireDate(@Param("jobNum") String jobNum,@Param("type")Integer type);

	/**
	 * 查询未同步成功数据，按失败次数排序，每次返回一条
	 * @param endDate
	 * @return
	 */
	List<UserSpacePO> queryUnSyncData(String endDate);

	/**
	 * 查询定时日期之前的数据
	 * @param scheduleDate
	 * @return
	 */
	List<UserSpacePO> queryUnsyncBeforeScheduleDate(String scheduleDate);

}

