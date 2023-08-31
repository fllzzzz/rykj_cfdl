package com.cf.parking.services.utils;

import cn.hutool.core.date.DateUnit;
import cn.hutool.core.date.DateUtil;
import org.apache.commons.collections4.CollectionUtils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.util.*;

public class TimeSlotCalculator {
    /**
     * 根据小时数和排班时间计算可用时间段
     *
     * @param hours     小时数
     * @param shiftDate 排班时间
     * @return 返回可用时间段的起始时间和结束时间
     */
    public static List<Date[]> calculateTimeSlot(int hours, List<Date> shiftDate) {
        Date date = DateUtil.beginOfHour(new Date());
        List<Date[]> resultList = new ArrayList<>();
        if (CollectionUtils.isEmpty(shiftDate)) {
            return resultList;
        }
        while (hours != 0) {
            if (DateUtil.compare(date, shiftDate.get(0)) < 0) {
                hours = 0;
            }
            Date finalDate = date;
            if (shiftDate.stream().anyMatch(o -> DateUtil.isSameDay(o, DateUtil.offsetSecond(finalDate, -1)))) {
                Date startDate = DateUtil.isSameTime(DateUtil.beginOfDay(finalDate), finalDate) ?
                        DateUtil.beginOfDay(DateUtil.offsetDay(finalDate, -1)) : DateUtil.beginOfDay(finalDate);
                long hour = DateUtil.between(startDate, finalDate, DateUnit.HOUR);
                if (hour >= hours) {
                    startDate = DateUtil.offsetHour(finalDate, -hours);

                    hours = 0;
                } else {
                    hours = (int) (hours - hour);
                }
                resultList.add(new Date[]{startDate, date});
                date = startDate;

            } else {
                date = DateUtil.isSameTime(DateUtil.beginOfDay(finalDate), finalDate) ?
                        DateUtil.beginOfDay(DateUtil.offsetDay(finalDate, -1)) : DateUtil.beginOfDay(finalDate);
            }

        }


        return resultList;
    }

    /**
     * 判断两个时间段是否有重叠
     *
     * @param start1 第一个时间段的开始时间
     * @param end1   第一个时间段的结束时间
     * @param start2 第二个时间段的开始时间
     * @param end2   第二个时间段的结束时间
     * @return 如果有重叠的时间段，返回true；否则返回false
     */
    public static boolean isOverlap(Date start1, Date end1, Date start2, Date end2) {
        return start1.before(end2) && end1.after(start2);
    }


    // 测试
    public static void main(String[] args) {
        List<String> shiftDateList = Arrays.asList(
                "2023-03-22", "2023-03-23", "2023-03-24",
                "2023-03-26", "2023-03-28", "2023-03-29"
        );
        long between = DateUtil.between(DateUtil.beginOfDay(new Date()), DateUtil.endOfDay(new Date()), DateUnit.HOUR);
        int hours = 49;
        List<Date> dateList = new ArrayList<>();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        for (String dateString : shiftDateList) {
            Date date = null;
            try {
                date = sdf.parse(dateString);
            } catch (ParseException e) {
                throw new RuntimeException(e);
            }
            dateList.add(date);
        }
        List<Date[]> dates = calculateTimeSlot(49, dateList);

        System.out.println("可用时间段：");

    }
}

