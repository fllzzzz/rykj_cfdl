package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.DingNoticeRecordPO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author whx
 * @date 2023-03-31 10:13:21
 * @description 钉钉通知记录表
 */
@Mapper
public interface DingNoticeRecordMapper extends BaseMapper<DingNoticeRecordPO> {

	/**
	 * 批量保存，返回主键ID
	 *
	 * @param list
	 * @return
	 */
	Integer insertBatchNotice(@Param("list") List<DingNoticeRecordPO> list);
}

