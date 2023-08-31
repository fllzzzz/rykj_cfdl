package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.CrossRecordsPO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author lpy
 * @date 2023-03-28 14:05:52
 * @description 过车记录表
 */
@Mapper
public interface CrossRecordsMapper extends BaseMapper<CrossRecordsPO> {

	/**
	 * replace into 新增数据
	 *
	 * @param list
	 */
	void replaceCrossRecords(@Param("list") List<CrossRecordsPO> list);
}

