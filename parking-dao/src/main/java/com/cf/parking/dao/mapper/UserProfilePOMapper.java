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
	 * @param userIds
	 * @return
	 */
    Integer batchSetDefaultParkingLotByUserIds(List<Long> userIds,String defaultParkingLot);
}