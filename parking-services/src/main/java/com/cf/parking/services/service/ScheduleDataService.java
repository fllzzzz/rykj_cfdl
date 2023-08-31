package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import cn.hutool.core.date.DateField;
import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.po.ScheduleDataPO;
import com.cf.parking.dao.mapper.ScheduleDataMapper;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.annotation.Resource;


/**
 * @author whx
 * @date 2023-03-27 09:31:03
 * @description 排班记录表
 */
@Service
public class ScheduleDataService extends ServiceImpl<ScheduleDataMapper, ScheduleDataPO> implements IService<ScheduleDataPO> {

	@Resource
	private ScheduleDataMapper scheduleDataMapper;

	/**
	 * 查询这段时间的数据数
	 *
	 * @param startDate
	 * @param endDate
	 * @return
	 */
	public long countSeasonData(Date startDate, Date endDate) {
		LambdaQueryWrapper<ScheduleDataPO> queryWrapper = new LambdaQueryWrapper<ScheduleDataPO>()
				.between(ScheduleDataPO::getShiftDate, startDate, endDate);
		return this.count(queryWrapper);
	}

	/**
	 * 查询这段时间的排班数据
	 *
	 * @return
	 */
	public List<ScheduleDataPO> getScheduleDate(Date startDate, Date endDate) {
		return scheduleDataMapper.selectList(new LambdaQueryWrapper<ScheduleDataPO>().ne(ScheduleDataPO::getHours, "0.00")
				.between(ScheduleDataPO::getShiftDate, startDate, endDate).orderByDesc(ScheduleDataPO::getShiftDate));
	}

    /**
     * 获取排班表30天前得排班数据
     * @return
     */
    public List<Date> getWorkDayByThirtyDay(){
        return this.list(new LambdaQueryWrapper<ScheduleDataPO>().ne(ScheduleDataPO::getHours,"0.00")
                .between(ScheduleDataPO::getShiftDate,  DateUtil.offset(new Date(), DateField.DAY_OF_YEAR, -30),new Date()))
                .stream().map(ScheduleDataPO::getShiftDate).collect(Collectors.toList());
    }
}