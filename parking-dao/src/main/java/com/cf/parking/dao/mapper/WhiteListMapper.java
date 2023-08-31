package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.WhiteListPO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author whx
 * @date 2023-03-30 11:02:41
 * @description 白名单记录表
 */
@Mapper
public interface WhiteListMapper extends BaseMapper<WhiteListPO> {

	/**
	 * replace into 新增数据
	 *
	 * @param list
	 */
	void replaceWhiteList(@Param("list") List<WhiteListPO> list);

}