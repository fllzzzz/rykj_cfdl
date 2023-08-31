package com.cf.parking.services.service;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.UserSpaceMapper;
import com.cf.parking.dao.po.UserSpacePO;
import com.cf.parking.facade.dto.UserSpaceDTO;
import com.cf.parking.facade.dto.UserSpaceFuncTimeDTO;
import com.cf.parking.facade.dto.UserSpacePageDTO;
import com.cf.parking.facade.dto.UserSpaceValidityDTO;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author lpy
 * @date 2023-03-27 16:56:05
 * @description 用户车位表
 */
@Service
public class UserSpaceService extends ServiceImpl<UserSpaceMapper, UserSpacePO> implements IService<UserSpacePO> {
    // 每次批量保存最大的数量
    private final Integer MAX_BATCH_SAVE_NUM = 500;
    private final String DATE_FORMAT_STR = "yyyy-MM-dd";

    @Resource
    private UserSpaceService userSpaceService;
    @Resource
    private UserSpaceMapper userSpaceMapper;

    /**
     * 获取请求数据，返回数据总大小
     *
     * @param data data
     * @return total
     */
    public List<UserSpacePO> getUserSpaceList(PageResponse<UserSpaceDTO> data) {

        List<UserSpaceDTO> list = data.getList();

        LinkedList<UserSpacePO> spacePOS = new LinkedList<>();
        for (UserSpaceDTO userSpaceDTO : list) {
            UserSpacePO userSpacePO = new UserSpacePO()
                    .setPlateNo(userSpaceDTO.getPlateNo())
                    .setJobNumber(userSpaceDTO.getPersonId())
                    .setName(userSpaceDTO.getPersonName());
            List<UserSpaceValidityDTO> validityList = userSpaceDTO.getValidity();

            // validityList可能为空
            if (CollectionUtils.isEmpty(validityList)) {
                continue;
            }

            StringBuffer sb = new StringBuffer();
            String startTime = "";
            String endTime = "";

            // 一般大小就一个
            for (UserSpaceValidityDTO userSpaceValidityDTO : validityList) {
                sb.append(userSpaceValidityDTO.getParkName()).append(",");

                List<UserSpaceFuncTimeDTO> functionTime = userSpaceValidityDTO.getFunctionTime();
                UserSpaceFuncTimeDTO eachFuncTime = functionTime.get(0);
                // funcTime数组就一个大小
                startTime = eachFuncTime.getStartTime();
                endTime = eachFuncTime.getEndTime();
            }
            userSpacePO.setStartDate(DateUtil.parse(startTime, DATE_FORMAT_STR));
            DateTime endDate = DateUtil.parse(endTime, DATE_FORMAT_STR);
            userSpacePO.setEndDate(endDate);
            // 精确到天，结束时间大于等于当前时间
            if (!this.judgeDateBefore(new Date(), endDate)) {
                continue;
            }
            // 如果多个车场拼接,去掉最后一个逗号
            String parkingLots = sb.toString();
            if (parkingLots.endsWith(",")) {
                parkingLots = parkingLots.substring(0, parkingLots.length() - 1);
            }
            userSpacePO.setParkingLot(parkingLots);
            spacePOS.add(userSpacePO);
        }
        return spacePOS;
    }


    /**
     * 分页查询 更新时间倒序
     * 所属车场（模糊）、工号、姓名、车牌号、有效起始日期、有效截止日期、
     *
     * @param param
     * @return
     */
    public IPage getUserSpacePage(UserSpacePageDTO param) {
        LambdaQueryWrapper<UserSpacePO> queryWrapper = new LambdaQueryWrapper<UserSpacePO>()
                .like(!StringUtils.isEmpty(param.getParkingLot()), UserSpacePO::getParkingLot, param.getParkingLot())
                .eq(!StringUtils.isEmpty(param.getJobNumber()), UserSpacePO::getJobNumber, param.getJobNumber())
                .eq(!StringUtils.isEmpty(param.getPlateNo()), UserSpacePO::getPlateNo, param.getPlateNo())
                .le(!ObjectUtils.isEmpty(param.getEndDate()), UserSpacePO::getEndDate, param.getEndDate())
                .orderByDesc(UserSpacePO::getUpdateTm);
        Page page = new Page().setCurrent(param.getPageNo()).setSize(param.getPageSize());
        return userSpaceMapper.selectPage(page, queryWrapper);
    }

    /**
     * 列表查询 更新时间倒序
     * 所属车场（模糊）、工号、姓名、车牌号、有效起始日期、有效截止日期、
     *
     * @param param
     * @return
     */
    public List<UserSpacePO> getUserSpaceList(UserSpacePageDTO param) {

        return userSpaceMapper.selectList(new LambdaQueryWrapper<UserSpacePO>()
                .like(!StringUtils.isEmpty(param.getParkingLot()), UserSpacePO::getParkingLot, param.getParkingLot())
                .eq(!StringUtils.isEmpty(param.getJobNumber()), UserSpacePO::getJobNumber, param.getJobNumber())
                .eq(!StringUtils.isEmpty(param.getPlateNo()), UserSpacePO::getPlateNo, param.getPlateNo())
                .ge(!ObjectUtils.isEmpty(param.getStartDate()), UserSpacePO::getStartDate, param.getStartDate())
                .le(!ObjectUtils.isEmpty(param.getEndDate()), UserSpacePO::getEndDate, param.getEndDate())
                .orderByDesc(UserSpacePO::getUpdateTm));
    }

    /**
     * 分批保存 500一批
     *
     * @param list
     */
    private void saveData(List<UserSpacePO> list) {
        // 分批保存
        for (int i = 0; i < list.size(); i += MAX_BATCH_SAVE_NUM) {
            List<UserSpacePO> batchList = list.subList(i, Math.min(i + MAX_BATCH_SAVE_NUM, list.size()));
            userSpaceService.saveBatch(batchList);
        }
    }

    // 四楼
    //  四楼，五楼，产品

    /**
     * 将车牌号多个车场去重合并  (1:四楼，五楼，产品 2:五楼，产品 3：老院区）
     *
     * @param list
     */
    public void mergeParkingLots(List<UserSpacePO> list) {
        Map<String, List<UserSpacePO>> collect = list.stream().distinct().collect(Collectors.groupingBy(UserSpacePO::getPlateNo));
        ArrayList<UserSpacePO> resultList = new ArrayList<>(2000);
        collect.forEach((key, values) -> {
            Set<String> parkingLots = values.stream().map(o -> o.getParkingLot()).collect(Collectors.toSet());
            LinkedHashSet<String> resultParkingLots = new LinkedHashSet<>();
            parkingLots.forEach(o -> {
                String[] splitLots = o.split(",");
                resultParkingLots.addAll(Arrays.asList(splitLots));
            });

            String parkingLotsString = parkingLots.stream().collect(Collectors.joining(","));
            UserSpacePO spacePO = BeanConvertorUtils.map(values.get(0), UserSpacePO.class);
            spacePO.setParkingLot(parkingLotsString);
            resultList.add(spacePO);
        });
        this.saveData(resultList);
    }

    /**
     * 判断endTime是否大于等于今天  精确到天
     *
     * @param now
     * @param endDate
     * @return true:结束时间大于当前时间
     */
    private boolean judgeDateBefore(Date now, Date endDate) {
        String nowStr = DateUtil.format(now, DATE_FORMAT_STR);
        // 精确到天
        DateTime nowDate = DateUtil.parse(nowStr, DATE_FORMAT_STR);
        // 判断
        if (endDate.compareTo(nowDate) >= 0) {
            return true;
        }
        return false;
    }
}

