package com.cf.parking.services.facade.impl;

import cn.hutool.core.date.DateUnit;
import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.cf.parking.dao.po.*;
import com.cf.parking.facade.dto.*;
import com.cf.parking.facade.enums.IsDeleteEnum;
import com.cf.parking.facade.facade.BlackListFacade;
import com.cf.parking.facade.facade.DingNoticeRecordFacade;
import com.cf.parking.facade.facade.ParkingFacade;
import com.cf.parking.services.integration.GatewayHikvisionFeign;
import com.cf.parking.services.service.*;
import com.cf.parking.services.utils.TimeSlotCalculator;
import com.cf.support.result.PageResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;


@Service
@Slf4j
public class ParkingFacadeImpl implements ParkingFacade {
    @Resource
    private ScheduleDataService ScheduleDataService;

    @Resource
    private NoticeRecordService noticeRecordService;
    @Resource
    private BlacklistSettingsService blacklistSettingsService;
    @Resource
    private BlackListService blackListService;
    @Resource
    private WhiteListService whiteListService;
    @Resource
    private GatewayHikvisionFeign gatewayHikvisionFeign;
    @Resource
    private UserSpaceService userSpaceService;
    @Resource
    private CrossRecordsService crossRecordsService;

    @Resource
    private NotParkingRecordService notParkingRecordService;
    @Resource
    private NotParkingNoticeRecordService notParkingNoticeRecordService;
    @Resource
    private BlackListFacade blackListFacade;
    @Resource
    private DingNoticeRecordFacade dingNoticeRecordFacade;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void dealZombieVehicle() {
        List<BlacklistSettingsPO> list = blacklistSettingsService.list();
        if (CollectionUtils.isEmpty(list)) {
            log.warn("黑名单设置表未配置，暂时不开始");
            return;
        }
        BlacklistSettingsPO blacklistSettingsPO = list.get(0);
        //todo 从海康那边查询接口
        CarInRecordQueryDTO carInRecordQueryDTO = new CarInRecordQueryDTO().setParkTime(String.valueOf(blacklistSettingsPO.getNoRecordInTime())).setPageSize(1000).setPageNo(1);
        HikvisionResult<PageResponse<CarInRecordDTO>> pageResponseResult = gatewayHikvisionFeign.tempCarInRecords("artemis/api/pms/v1/tempCarInRecords/page", carInRecordQueryDTO);
        List<CarInRecordDTO> carInRecordDTOS = pageResponseResult.getData().getList();
        carInRecordDTOS = carInRecordDTOS.stream().filter(o -> !"无车牌".equals(o.getPlateNo())).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(carInRecordDTOS)) {
            return;
        }
        //黑名单
        List<String> blackList = blackListService.getBlackList();
        //白名单
        List<String> whiteList = whiteListService.getWhiteList();
        carInRecordDTOS = carInRecordDTOS.stream().filter(obj -> !blackList.contains(obj.getPlateNo())).collect(Collectors.toList());
        carInRecordDTOS = carInRecordDTOS.stream().filter(obj -> !whiteList.contains(obj.getPlateNo())).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(carInRecordDTOS)) {
            return;
        }
        List<UserSpacePO> userSpacePOList = userSpaceService.list();
        Map<String, UserSpacePO> userSpaceByPlantNoMap = userSpacePOList.stream()
                .collect(Collectors.toMap(UserSpacePO::getPlateNo, userSpacePO -> userSpacePO));
        //todo 重复数据要不要处理
        List<NoticeRecordPO> noticeRecordPOList = noticeRecordService.list
                (new LambdaQueryWrapper<NoticeRecordPO>().eq(NoticeRecordPO::getIsDelete, 0));
        List<NoticeRecordPO> updateList = new ArrayList<>();
        Map<String, List<NoticeRecordPO>> groupByJobNumber = noticeRecordPOList.
                stream().collect(Collectors.groupingBy(NoticeRecordPO::getJobNumber));
        List<NoticeRecordPO> addList = new LinkedList<>();
        StringBuilder messageJobNumber = new StringBuilder();
        carInRecordDTOS = carInRecordDTOS.stream().filter(o -> StringUtils.isNotBlank(o.getPlateNo()))
                .sorted(Comparator.comparing(CarInRecordDTO::getInTime))
                .collect(Collectors.collectingAndThen(Collectors.toCollection
                        (() -> new TreeSet<>(Comparator.comparing(CarInRecordDTO::getPlateNo))), ArrayList::new));
        List<String> addBlackList = new ArrayList<>();
        List<DingNoticeRecordDTO> dingNoticeRecordDTOS = new LinkedList<>();

        for (CarInRecordDTO o : carInRecordDTOS) {
            int lastPeekHour = convertToHours(o.getParkTime());
            UserSpacePO userSpacePO = userSpaceByPlantNoMap.get(o.getPlateNo());
            if (ObjectUtils.isNotEmpty(userSpacePO) && StringUtils.isNotBlank(userSpacePO.getJobNumber())) {
                List<NoticeRecordPO> noticeRecordList = groupByJobNumber.get(userSpacePO.getJobNumber());
                if (CollectionUtils.isEmpty(noticeRecordList)) {
                    addList.add(new NoticeRecordPO().setIsDelete(blacklistSettingsPO.getCumulativeNumber() == 1 ?
                                    IsDeleteEnum.TRUE.getCode() : IsDeleteEnum.FALSE.getCode())
                            .setInRecordSyscode(o.getInRecordSyscode())
                            .setLastPeekHour(lastPeekHour).setJobNumber(userSpacePO.getJobNumber()));
                    String message;
                    if (blacklistSettingsPO.getCumulativeNumber() == 1) {

                        message = userSpacePO.getName() + "(" + userSpacePO.getJobNumber() + ")" +
                                "您车辆入场权限已被删除，暂时无权限进停车场，如为误报，请及时与管理员反馈";
                        messageJobNumber.append(userSpacePO.getJobNumber()).append(",");
                        addBlackList.add(userSpacePO.getJobNumber());
                    } else {
                        message = userSpacePO.getName() + "(" + userSpacePO.getJobNumber() + ")" +
                                "您" +
                                blacklistSettingsPO.getNoRecordInTime() + "小时内无出库记录，将被记录为僵尸车，超过" +
                                blacklistSettingsPO.getCumulativeNumber() + "次，您的车辆入场权限将被删除，如为误报，请及时与管理员反馈。";
                    }
                    DingNoticeRecordDTO dingNoticeRecordDTO = new DingNoticeRecordDTO().
                            setMessage(message).setJobNumber(userSpacePO.getJobNumber());
                    dingNoticeRecordDTOS.add(dingNoticeRecordDTO);


                } else {
                    int sum = noticeRecordList.stream().mapToInt(NoticeRecordPO::getNoticeNumber).sum();
                    AtomicBoolean isNeedAdd = new AtomicBoolean(true);
                    boolean isNeedSendMessage = noticeRecordList.stream()
                            .anyMatch(noticeRecordPO -> (noticeRecordPO.getInRecordSyscode()
                                    .equals(o.getInRecordSyscode()) &&
                                    noticeRecordPO.getLastPeekHour() +
                                            blacklistSettingsPO.getNoRecordInTime() <= lastPeekHour))
                            || noticeRecordList.stream().noneMatch(noticeRecordPO ->
                            noticeRecordPO.getInRecordSyscode().equals(o.getInRecordSyscode()));

                    noticeRecordList.forEach(noticeRecordPO -> {
                        if (noticeRecordPO.getInRecordSyscode().equals(o.getInRecordSyscode()) &&
                                noticeRecordPO.getLastPeekHour() +
                                        blacklistSettingsPO.getNoRecordInTime() <= lastPeekHour) {
                            noticeRecordPO.setNoticeNumber(noticeRecordPO.getNoticeNumber() + 1);
                            noticeRecordPO.setUpdateTm(new Date());
                            noticeRecordPO.setLastPeekHour(lastPeekHour);
                            noticeRecordPO.setIsDelete(sum + 1 >= blacklistSettingsPO.getCumulativeNumber() ? 1 : 0);
                            updateList.add(noticeRecordPO);
                        } else if (isNeedSendMessage && sum + 1 >= blacklistSettingsPO.getCumulativeNumber()) {
                            noticeRecordPO.setIsDelete(1);
                            updateList.add(noticeRecordPO);
                        }
                        if (noticeRecordPO.getInRecordSyscode().equals(o.getInRecordSyscode())) {
                            isNeedAdd.set(false);
                        }

                    });
                    if (isNeedAdd.get()) {
                        addList.add(new NoticeRecordPO().setIsDelete(sum + 1 >=
                                        blacklistSettingsPO.getCumulativeNumber() ? 1 : 0)
                                .setInRecordSyscode(o.getInRecordSyscode())
                                .setLastPeekHour(lastPeekHour).setJobNumber(userSpacePO.getJobNumber()));
                    }

                    if (isNeedSendMessage) {

                        String message;
                        if (sum + 1 >= blacklistSettingsPO.getCumulativeNumber()) {


                            addBlackList.add(userSpacePO.getJobNumber());
                            message = userSpacePO.getName() + "(" + userSpacePO.getJobNumber() + ")" + "您车辆入场权限已被删除，暂时无权限进停车场，如为误报，请及时与管理员反馈";
                            messageJobNumber.append(userSpacePO.getJobNumber()).append(",");
                        } else {
                            message = userSpacePO.getName() + "(" + userSpacePO.getJobNumber() + ")" + "您"
                                    + blacklistSettingsPO.getNoRecordInTime() + "小时内无出库记录，将被记录为僵尸车，超过" +
                                    blacklistSettingsPO.getCumulativeNumber() + "次，您的车辆入场权限将被删除，如为误报，请及时与管理员反馈。";

                        }
                        DingNoticeRecordDTO dingNoticeRecordDTO = new DingNoticeRecordDTO().
                                setMessage(message).setJobNumber(userSpacePO.getJobNumber());
                        dingNoticeRecordDTOS.add(dingNoticeRecordDTO);
                    }
                }
            }

        }

        if (CollectionUtils.isNotEmpty(addList)) {
            noticeRecordService.saveBatch(addList);
        }
        if (CollectionUtils.isNotEmpty(updateList)) {
            noticeRecordService.updateBatchById(updateList);
        }
        Map<String, List<UserSpacePO>> userGroupByJobNumber = userSpacePOList.stream().collect(Collectors.groupingBy(UserSpacePO::getJobNumber));
        List<BlackListBatchAdditionDTO> blackListBatchAdditionDTOS = new LinkedList<>();
        addBlackList.stream().distinct().collect(Collectors.toList()).forEach(o -> {
            List<UserSpacePO> userSpaceList = userGroupByJobNumber.get(o);
            if (CollectionUtils.isNotEmpty(userSpaceList)) {
                blackListBatchAdditionDTOS.addAll(userSpaceList.stream().map(userSpacePO -> new
                        BlackListBatchAdditionDTO().setJobNumber(userSpacePO.getJobNumber()).setName(userSpacePO.getName())
                        .setJoinReason("僵尸车")
                        .setPlateNo(userSpacePO.getPlateNo())).collect(Collectors.toList()));
            }
        });
        if (CollectionUtils.isNotEmpty(blackListBatchAdditionDTOS)) {
            blackListFacade.blackListAddition(blackListBatchAdditionDTOS);
        }
        if (CollectionUtils.isNotEmpty(dingNoticeRecordDTOS)) {
            if (StringUtils.isNotBlank(messageJobNumber.toString())) {
                messageJobNumber = new StringBuilder(messageJobNumber.substring(0, messageJobNumber.length() - 1));
                dingNoticeRecordDTOS.add(new DingNoticeRecordDTO().setMessage(messageJobNumber +
                        "系统已加入停车系统黑名单，请知晓！").setJobNumber("CFDL09860"));
            }
            dingNoticeRecordFacade.dingNotify(dingNoticeRecordDTOS);
        }
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public void dealNoParking() {
        List<BlacklistSettingsPO> list = blacklistSettingsService.list();
        if (CollectionUtils.isEmpty(list)) {
            log.warn("黑名单设置表未配置，暂时不开始");
            return;
        }
        BlacklistSettingsPO blacklistSettingsPO = list.get(0);

        List<Date> workDayByThirtyDay = ScheduleDataService.getWorkDayByThirtyDay().stream()
                .sorted(Comparator.comparing(Date::getTime))
                .collect(Collectors.toList());
        List<Date[]> dates = TimeSlotCalculator.calculateTimeSlot(blacklistSettingsPO.getNoRecordInTime(), workDayByThirtyDay);
        if (CollectionUtils.isEmpty(dates)) {
            log.warn("排班表未配置");
            return;
        }
        List<UserSpacePO> userSpacePOList = userSpaceService.list();
        userSpacePOList = userSpacePOList.stream().
                filter(o -> DateUtil.between(o.getStartDate(), new Date(), DateUnit.HOUR)
                        >= blacklistSettingsPO.getNoRecordInTime()).collect(Collectors.toList());
        NotParkingRecordPO notParkingRecordPO = notParkingRecordService.getOne(new LambdaQueryWrapper<NotParkingRecordPO>().orderByDesc(NotParkingRecordPO::getCreateTm).last(" limit 1"));


        if (ObjectUtils.isEmpty(notParkingRecordPO) || !TimeSlotCalculator.isOverlap(notParkingRecordPO.getStartTm(),
                notParkingRecordPO.getEndTm(), dates.get(dates.size() - 1)[0], dates.get(0)[1])) {
            notParkingRecordService.save(new NotParkingRecordPO().setStartTm(dates.get(dates.size() - 1)[0]).setEndTm(dates.get(0)[1]));
            LambdaQueryWrapper<CrossRecordsPO> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.select(CrossRecordsPO::getPlateNo).between(CrossRecordsPO::getCrossTime, dates.get(dates.size() - 1)[0], dates.get(0)[1]);
            List<String> parkedCarList = crossRecordsService.list(queryWrapper.groupBy(CrossRecordsPO::getPlateNo)).stream().
                    map(CrossRecordsPO::getPlateNo).collect(Collectors.toList());

            List<String> blackList = blackListService.getBlackList();
            List<String> whiteList = whiteListService.getWhiteList();

            //todo 从海康那边查询接口
            CarInRecordQueryDTO carInRecordQueryDTO = new CarInRecordQueryDTO().setParkTime(String.valueOf(blacklistSettingsPO.getNoRecordInTime())).setPageSize(1000).setPageNo(1);
            HikvisionResult<PageResponse<CarInRecordDTO>> pageResponseResult = gatewayHikvisionFeign.tempCarInRecords("artemis/api/pms/v1/tempCarInRecords/page", carInRecordQueryDTO);
            List<String> collect = pageResponseResult.getData().getList().stream().map(CarInRecordDTO::getPlateNo).collect(Collectors.toList());
            parkedCarList.addAll(collect);
            parkedCarList = parkedCarList.stream().distinct().collect(Collectors.toList());
            List<String> finalParkedCarList = parkedCarList;

            List<String> parkedJobNumberList = userSpacePOList.stream().filter(o ->
                            finalParkedCarList.contains(o.getPlateNo()) || blackList.contains(o.getPlateNo()) || whiteList.contains(o.getPlateNo()))
                    .map(UserSpacePO::getJobNumber).collect(Collectors.toList());
            List<String> noParkJobNumberList = userSpacePOList.stream().map(UserSpacePO::getJobNumber)
                    .filter(jobNumber ->
                            !parkedJobNumberList.contains(jobNumber)).distinct().collect(Collectors.toList());
            if (CollectionUtils.isEmpty(noParkJobNumberList)) {
                return;
            }
            List<NotParkingNoticeRecordPO> notParkingNoticeRecordPOList = notParkingNoticeRecordService.
                    list(new LambdaQueryWrapper<NotParkingNoticeRecordPO>()
                            .in(NotParkingNoticeRecordPO::getJobNumber, noParkJobNumberList)
                            .eq(NotParkingNoticeRecordPO::getIsDelete, IsDeleteEnum.FALSE.getCode()));
            Map<String, NotParkingNoticeRecordPO> groupByJobNumber = notParkingNoticeRecordPOList.stream()
                    .collect(Collectors.toMap(NotParkingNoticeRecordPO::getJobNumber, o -> o));

            Map<String, List<UserSpacePO>> userGroupByJobNumber = userSpacePOList.stream().collect(Collectors.groupingBy(UserSpacePO::getJobNumber));
            List<NotParkingNoticeRecordPO> updateList = new ArrayList<>();
            List<BlackListBatchAdditionDTO> addBlackList = new LinkedList<>();
            AtomicReference<String> messageJobNumber = new AtomicReference<>("");
            List<DingNoticeRecordDTO> dingNoticeRecordDTOS = new LinkedList<>();
            List<NotParkingNoticeRecordPO> addList = noParkJobNumberList.stream().map(o -> {
                NotParkingNoticeRecordPO notParkingNoticeRecordPO = groupByJobNumber.get(o);
                String message;
                List<UserSpacePO> userSpaceList = userGroupByJobNumber.get(o);

                if (ObjectUtils.isNotEmpty(notParkingNoticeRecordPO)) {
                    notParkingNoticeRecordPO.setNoticeNumber(notParkingNoticeRecordPO.getNoticeNumber() + 1).setUpdateTm(new Date());

                    if (notParkingNoticeRecordPO.getNoticeNumber() >= blacklistSettingsPO.getCumulativeNumber()) {
                        notParkingNoticeRecordPO.setIsDelete(IsDeleteEnum.TRUE.getCode());
                        addBlackList.addAll(
                                userSpaceList.stream().map(userSpacePO -> new
                                        BlackListBatchAdditionDTO().setJobNumber(userSpacePO.getJobNumber()).setName(userSpacePO.getName())
                                        .setJoinReason("不停车")
                                        .setPlateNo(userSpacePO.getPlateNo())).collect(Collectors.toList()));
                        message = o + "您车辆入场权限已被删除，暂时无权限进停车场，如为误报，请及时与管理员反馈";
                        messageJobNumber.set(messageJobNumber.get() + o + ",");
                    } else {
                        message = o + "您" + blacklistSettingsPO.getNoRecordInTime() + "小时内无进出库记录，将被记录为无停车刚需，超过" +
                                blacklistSettingsPO.getCumulativeNumber() + "次，您的车辆入场权限将被删除，如为误报，请及时与管理员反馈。";
                    }
                    updateList.add(notParkingNoticeRecordPO);
                    DingNoticeRecordDTO dingNoticeRecordDTO = new DingNoticeRecordDTO().
                            setMessage(message).setJobNumber(o);
                    dingNoticeRecordDTOS.add(dingNoticeRecordDTO);
                    return null;
                }
                NotParkingNoticeRecordPO notParkingNoticeRecord = new NotParkingNoticeRecordPO().setJobNumber(o);
                if (blacklistSettingsPO.getCumulativeNumber().equals(1)) {
                    addBlackList.addAll(
                            userSpaceList.stream().map(userSpacePO -> new
                                    BlackListBatchAdditionDTO().setJobNumber(userSpacePO.getJobNumber()).setName(userSpacePO.getName())
                                    .setJoinReason("不停车")
                                    .setPlateNo(userSpacePO.getPlateNo())).collect(Collectors.toList()));
                    notParkingNoticeRecord.setIsDelete(IsDeleteEnum.TRUE.getCode());
                    message = o + "您车辆入场权限已被删除，暂时无权限进停车场，如为误报，请及时与管理员反馈";
                    messageJobNumber.set(messageJobNumber.get() + o + ",");
                } else {
                    message = o + "您" + blacklistSettingsPO.getNoRecordInTime() + "小时内无进出库记录，将被记录为无停车刚需，超过" +
                            blacklistSettingsPO.getCumulativeNumber() + "次，您的车辆入场权限将被删除，如为误报，请及时与管理员反馈。";
                }
                DingNoticeRecordDTO dingNoticeRecordDTO = new DingNoticeRecordDTO().
                        setMessage(message).setJobNumber(o);
                dingNoticeRecordDTOS.add(dingNoticeRecordDTO);
                return notParkingNoticeRecord;
            }).filter(ObjectUtils::isNotEmpty).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(addList)) {
                notParkingNoticeRecordService.saveBatch(addList);
            }
            if (CollectionUtils.isNotEmpty(updateList)) {
                notParkingNoticeRecordService.updateBatchById(updateList);
            }
            if (CollectionUtils.isNotEmpty(addBlackList)) {
                blackListFacade.blackListAddition(addBlackList);
            }
            if (CollectionUtils.isNotEmpty(dingNoticeRecordDTOS)) {
                if (StringUtils.isNotBlank(messageJobNumber.get())) {
                    String message = messageJobNumber.get().substring(0, messageJobNumber.get().length() - 1);
                    dingNoticeRecordDTOS.add(new DingNoticeRecordDTO().setMessage(message +
                            "系统已加入停车系统黑名单，请知晓！").setJobNumber("CFDL09860"));
                }
                dingNoticeRecordFacade.dingNotify(dingNoticeRecordDTOS);
            }

        }


    }


    /**
     * 将时间字符串转换为总小时数
     *
     * @param timeString 时间字符串，例如 "1天1小时1分钟"、"1小时1分钟"、"1分钟"
     * @return 总小时数
     */
    private static int convertToHours(String timeString) {

        // Define regex patterns for different time formats
        Pattern dayPattern = Pattern.compile("(\\d+)天");
        Pattern hourPattern = Pattern.compile("(\\d+)小时");
        Pattern minutePattern = Pattern.compile("(\\d+)分钟");

        // Initialize total hours to 0
        int totalHours = 0;

        // Match day, hour, and minute patterns and add up total hours
        Matcher dayMatcher = dayPattern.matcher(timeString);
        if (dayMatcher.find()) {
            int days = Integer.parseInt(dayMatcher.group(1));
            totalHours += days * 24;
        }

        Matcher hourMatcher = hourPattern.matcher(timeString);
        if (hourMatcher.find()) {
            int hours = Integer.parseInt(hourMatcher.group(1));
            totalHours += hours;
        }

        Matcher minuteMatcher = minutePattern.matcher(timeString);
        if (minuteMatcher.find()) {
            int minutes = Integer.parseInt(minuteMatcher.group(1));
            totalHours += minutes / 60.0;
        }
        return totalHours;
    }

}
