package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.UserProfilePO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface UserProfilePOMapper extends BaseMapper<UserProfilePO> {

	int insertSelective(UserProfilePO record);

	int updateSelection(@Param("record") UserProfilePO record);

	int updateSelectionList(@Param("list") List<UserProfilePO> record);

	/**
	 * 批量修改用户默认停车场
	 *
	 * @param userIds
	 * @return
	 */
    Integer batchSetDefaultParkingLotByUserIds(@Param("userIds") List<Long> userIds, @Param("defaultParkingLot") String defaultParkingLot);

	/**
	 * 根据工号设置停车场
	 * @param jobNumList
	 * @param parkingLot
	 */
	void batchSetParkingLotByJobNum(@Param("jobNumList")List<String> jobNumList,@Param("parkingLot") String parkingLot);
}