package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.NotParkingNoticeRecordPO;
import org.apache.ibatis.annotations.Mapper;

/**
 * @author lpy
 * @date 2023-03-29 16:41:26
 *
 * @description 不停车通知记录表
 */
@Mapper
public interface NotParkingNoticeRecordMapper extends BaseMapper<NotParkingNoticeRecordPO> {

}

