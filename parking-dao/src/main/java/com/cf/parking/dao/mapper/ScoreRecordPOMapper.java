package com.cf.parking.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.cf.parking.dao.po.ScoreRecordPO;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface ScoreRecordPOMapper extends BaseMapper<ScoreRecordPO> {
    int deleteByPrimaryKey(Long scoreRecordId);

    int insertSelective(ScoreRecordPO record);

    ScoreRecordPO selectByPrimaryKey(Long scoreRecordId);

    int updateByPrimaryKeySelective(ScoreRecordPO record);

    int updateByPrimaryKey(ScoreRecordPO record);

    int insertSelectiveList(@Param("list") List<ScoreRecordPO> record);
}