package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.UserVerifyPO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author whx
 * @date 2022-11-19 16:55:55
 * @description 车主认证表
 */
@Mapper
public interface UserVerifyMapper extends BaseMapper<UserVerifyPO> {

    /**
     * 批量审核
     * @param ids
     * @param state
     * @param reason
     */
    Integer batchAudit(@Param("ids") List<Long> ids, @Param("state") Integer state, @Param("reason") String reason);
    
    List<UserVerifyPO> queryVerifyListByUserIdList(List<Long> list);

	/**
	 * 根据id查询
	 * @param ids
	 * @return
	 */
	List<UserVerifyPO> selectListByIds(List<Long> list);
}

