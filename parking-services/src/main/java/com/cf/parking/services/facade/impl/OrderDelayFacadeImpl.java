package com.cf.parking.services.facade.impl;

import cn.hutool.core.date.DateField;
import cn.hutool.core.date.DateUnit;
import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.cf.parking.dao.mapper.CommonMarkPOMapper;
import com.cf.parking.dao.po.*;
import com.cf.parking.facade.bo.ScoreRecordBO;
import com.cf.parking.facade.constant.MessageConstant;
import com.cf.parking.facade.dto.CardMessageDTO;
import com.cf.parking.facade.dto.ParkingOrderDTO;
import com.cf.parking.facade.dto.TextMessageDTO;
import com.cf.parking.facade.enums.*;
import com.cf.parking.facade.facade.DingTalkMessageFacade;
import com.cf.parking.facade.facade.OrderDelayFacade;
import com.cf.parking.services.job.order.OrderDelayJobUtil;
import com.cf.parking.services.properties.DingTalkProperties;
import com.cf.parking.services.service.*;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @Classname OrderExtendFacadeImpl
 * @Date 2022/10/21 15:41
 * @Created by csy
 */
@Service
@Slf4j
public class OrderDelayFacadeImpl implements OrderDelayFacade {

    @Resource
    private DingTalkProperties dingTalkProperties;
    @Resource
    public OrderPeerService orderPeerService;
    @Resource
    public UserService userService;
    @Resource
    public ParkingEvaluateService parkingEvaluateService;
    @Resource
    public CommonMarkService commonMarkService;
    @Resource
    public CommonMarkPOMapper commonMarkPOMapper;
    @Autowired
    private ParkingOrderService parkingOrderService;
    @Autowired
    private DingTalkMessageFacade dingTalkMessageFacade;
    @Resource
    private OrderStateRecordService orderStateRecordService;

    @Resource
    private UserProfileService userProfileService;

    @Resource
    private ScoreRecordService scoreRecordService;

    @Value("${spring.profiles.active}")
    private String active;


    /**
     * 补偿机制，如果系统出现问题，未通知，小于这个时间的立即通知
     *
     * @param parkingOrderDTOList 订单列表
     */
    public void compensate(List<ParkingOrderDTO> parkingOrderDTOList) {
        //从查询的结果中过滤出需要立即执行的
        List<ParkingOrderDTO> needInstantlyOrderList = parkingOrderDTOList.stream().filter(OrderDTO -> !OrderDelayJobUtil.needNoticeOrderId.contains(OrderDTO.getParkingOrderId())
                && DateUtil.between(new Date(), OrderDTO.getOrderTime(), DateUnit.MINUTE) <= 15).collect(Collectors.toList());
        //处理异常宕机的情况
        needInstantlyOrderList.addAll(BeanConvertorUtils.copyList(parkingOrderService.list(new LambdaQueryWrapper<ParkingOrderPO>()
                .eq(ParkingOrderPO::getOrderState
                        , OrderStateEnum.ORDER_NOT_START.getCode()).le(ParkingOrderPO::getOrderTime, new Date())), ParkingOrderDTO.class));
        if (CollectionUtils.isNotEmpty(needInstantlyOrderList)) {
            this.dealOutTimeOrder(needInstantlyOrderList);
        }
    }

    @Override
    @Transactional
    public void delayDealOutTimeAndStartOrder(List<Long> orderIdList, Long runMinute) {
        //再判断一次，中间可能会被司机取消
        List<ParkingOrderPO> orderList = parkingOrderService.listByIds(orderIdList).stream().
                filter(o -> Objects.equals(o.getOrderState(), OrderStateEnum.ORDER_NOT_START.getCode())
                        || Objects.equals(o.getOrderState(), OrderStateEnum.ORDER_ING.getCode())).collect(Collectors.toList());

        this.dealOutTimeOrder(BeanConvertorUtils.copyList(orderList, ParkingOrderDTO.class));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void autoOutOrder() {
        List<ParkingOrderPO> parkingOrderPOList = parkingOrderService.list(new
                LambdaQueryWrapper<ParkingOrderPO>()
                .eq(ParkingOrderPO::getOrderState, OrderStateEnum.ORDER_ING.getCode())
                .le(ParkingOrderPO::getOrderTime, DateUtil.offset(new Date(), DateField.HOUR, -24)));
        if (CollectionUtils.isEmpty(parkingOrderPOList)) {
            return;
        }

        List<Long> needInstantlyOrderIdList = parkingOrderPOList.stream().
                map(ParkingOrderPO::getParkingOrderId).collect(Collectors.toList());

        //查询同行表确认和请求状态的同行单
        List<OrderPeerPO> orderPeerPOList = orderPeerService.
                getOrderPeerByOrderListAndStatusList(needInstantlyOrderIdList,
                        Collections.singletonList(OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode()));
        //更新订单状态
        parkingOrderService.update(new LambdaUpdateWrapper<ParkingOrderPO>()
                .in(ParkingOrderPO::getParkingOrderId, needInstantlyOrderIdList)
                .eq(ParkingOrderPO::getOrderState, OrderStateEnum.ORDER_ING.getCode())
                .set(ParkingOrderPO::getOrderState, OrderStateEnum.ORDER_OUT_TIME.getCode()));
        //更新停下状态
        orderPeerService.update(new LambdaUpdateWrapper<OrderPeerPO>()
                .in(OrderPeerPO::getParkingOrderId, needInstantlyOrderIdList).
                eq(OrderPeerPO::getRecordState, OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode())
                .set(OrderPeerPO::getRecordState, OrderPeerStateEnum.ORDER_PEER_OUT_TIME.getCode()));
        List<CardMessageDTO> cardMessageDTOList = new ArrayList<>(parkingOrderPOList.size());
        List<Long> userIdList = parkingOrderPOList.stream().map(ParkingOrderPO::getUserId).collect(Collectors.toList());
        Map<Long, UserPO> userByUserId = userService.getUserByUserIdList(userIdList).stream()
                .collect(Collectors.toMap(UserPO::getUserId, userProfile -> userProfile));
        List<OrderStateRecordPO> orderStateRecordPOList = parkingOrderPOList.stream().map(o -> {
            UserPO orDefault = userByUserId.getOrDefault(o.getUserId(), new UserPO());
            cardMessageDTOList.add(new CardMessageDTO().setMessage(MessageConstant.OUT_TIME_ORDER_MESSAGE)
                    .setUrl(dingTalkProperties.getOrderDetailUrl(o.getParkingOrderId()))
                    .setOpenIdList(Collections.singletonList(orDefault.getOpenId())));

            return new OrderStateRecordPO().setParkingOrderId(o.getParkingOrderId())
                    .setUserType(UserTypeEnum.AUTO.getCode())
                    .setUserId(0L)
                    .setOptType(OptTypeEnum.OUT_TIME.getCode());
        }).collect(Collectors.toList());
        orderStateRecordService.saveBatch(orderStateRecordPOList);
        dealEvaluate(parkingOrderPOList, orderPeerPOList);


        dingTalkMessageFacade.asyncSendBatchCard(cardMessageDTOList, MessageConstant.PARKING_TITLE);
        log.info("自动取消订单，订单号为[{}]", needInstantlyOrderIdList.stream().map(String::valueOf).collect(Collectors.joining(",")));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void automaticOrder() {
        //查询24小时还未评价的
        List<ParkingEvaluatePO> parkingEvaluatePOList = parkingEvaluateService.list(new LambdaQueryWrapper<ParkingEvaluatePO>()
                .eq(ParkingEvaluatePO::getEvaluateType, EvaluateTypeEnum.EVALUATE_DRIVER.getCode())
                .eq(ParkingEvaluatePO::getIsEvaluate, BooleanEnum.FALSE.getCode())
                .le(ParkingEvaluatePO::getCreateTm, DateUtil.offset(new Date(), DateField.HOUR, -24)));
        if (CollectionUtils.isEmpty(parkingEvaluatePOList)) {
            return;
        }
        List<Long> userId = parkingEvaluatePOList.stream().map(ParkingEvaluatePO::getEvaluateUserId).collect(Collectors.toList());
        Map<Long, List<ParkingEvaluatePO>> parkingEvaluateByUserId = parkingEvaluateService.list(new LambdaQueryWrapper<ParkingEvaluatePO>()
                .eq(ParkingEvaluatePO::getEvaluateType, EvaluateTypeEnum.EVALUATE_DRIVER.getCode())
                .eq(ParkingEvaluatePO::getIsEvaluate, BooleanEnum.TRUE.getCode())
                .in(ParkingEvaluatePO::getEvaluateUserId, userId)).stream().collect(Collectors.groupingBy(ParkingEvaluatePO::getEvaluateUserId));
        Map<Long, UserProfilePO> userByUserId = userProfileService.selectList(userId).stream().collect(Collectors.toMap(UserProfilePO::getUserId, userProfile -> userProfile));
        List<ScoreRecordPO> saveRecordList = new LinkedList<>();
        List<ParkingEvaluatePO> updateEvaluateList = new LinkedList<>();
        List<UserProfilePO> updateUserScoreList = parkingEvaluatePOList.stream().map(o -> {
            updateEvaluateList.add(new ParkingEvaluatePO()
                    .setParkingEvaluateId(o.getParkingEvaluateId()).setIsEvaluate(BooleanEnum.TRUE.getCode()).setLevel(5));
            UserProfilePO userProfilePO = userByUserId.getOrDefault(o.getEvaluateUserId(), new UserProfilePO());
            ScoreRecordPO scoreRecordPO = new ScoreRecordPO().setUserId(o.getEvaluateUserId()).setParkingOrderId(o.getParkingOrderId())
                    .setScore(10).setName(userProfilePO.getName()).setJobNumber(userProfilePO.getJobNumber());
            saveRecordList.add(scoreRecordPO);
            UserProfilePO updateUser = new UserProfilePO().setUserId(o.getEvaluateUserId()).setTotalScore(10);
            List<ParkingEvaluatePO> parkingEvaluatePOS = parkingEvaluateByUserId.get(o.getEvaluateUserId());
            if (CollectionUtils.isEmpty(parkingEvaluatePOS)) {
                updateUser.setDriveMark(5);
            } else {
                updateUser.setDriveMark
                        ((parkingEvaluatePOS.stream()
                                .mapToInt(ParkingEvaluatePO::getLevel).sum() + 5) / (parkingEvaluatePOS.size() + 1));
            }
            return updateUser;
        }).collect(Collectors.toList());

        parkingEvaluateService.updateBatchById(updateEvaluateList);
        userProfileService.updateSelectionList(updateUserScoreList);
        scoreRecordService.saveBatch(saveRecordList);
        log.info("自动评价订单完成,完成订单号列表为[{}]", parkingEvaluatePOList.stream()
                .map(o -> String.valueOf(o.getParkingOrderId()))
                .collect(Collectors.joining(",")));

    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void autoCountIntegral() {
        // 定时任务是下一个月一号触发，所以取上一个月
        Date countDate = DateUtil.lastMonth();
        if (!"prod".equals(active)) {
            // 方便测试取当月
            countDate = new Date();
        }

        // 判断当月是否已经触发过定时任务
        String code = DateFormatUtils.format(countDate, "yyyy-MM");
        CommonMarkPO commonMarkPO = new CommonMarkPO().setSourceCode(code).setSourceType(SourceTypeEnum.AUTO_COUNT_INTEGRAL_TASK.getCode()).setState(CommonMarkStateEnum.ALREADY_DEAL.getCode());
        if (ObjectUtils.isNotEmpty(commonMarkService.getRecordBySourceTypeSourceCodeAndState(commonMarkPO))) {
            return;
        }


        //司机统计
        List<ValidCountPO> driverCount = parkingOrderService.getValidCount(DateUtil.beginOfMonth(countDate)
                , DateUtil.endOfMonth(countDate), OrderStateEnum.ORDER_COMPLETE.getCode());
        //乘客统计
        List<ValidCountPO> passengerCount = orderPeerService.getValidCount(DateUtil.beginOfMonth(countDate)
                , DateUtil.endOfMonth(countDate), OrderPeerStateEnum.ORDER_PEER_COMPLETE.getCode());
        List<UserProfilePO> updateUserScoreList = new ArrayList<>();
        List<ScoreRecordPO> saveRecordList = new ArrayList<>();
        driverCount.forEach(driver -> {
            Long count = driver.getCount();
            if (!"prod".equals(active)) {
                //方便測試
                count = count * 5;
            }
            if (count >= 10) {
                this.getScoreChangeList(driver, updateUserScoreList, saveRecordList,
                        this.getDriverScoreByCount(count));
            }
        });
        passengerCount.forEach(passenger -> {
            Long count = passenger.getCount();
            if (!"prod".equals(active)) {
                //方便測試
                count = count * 5;
            }
            if (count >= 10) {
                this.getScoreChangeList(passenger, updateUserScoreList, saveRecordList,
                        this.getPassengerScoreByCount(count));
            }
        });
        if (CollectionUtils.isNotEmpty(updateUserScoreList)) {
            userProfileService.updateSelectionList(updateUserScoreList);

        }
        if (CollectionUtils.isNotEmpty(updateUserScoreList)) {
            scoreRecordService.saveBatch(saveRecordList);
        }

        // 有事物控制，直接插入一条数据，设置当前月份的状态为已处理
        commonMarkPOMapper.insertSelective(commonMarkPO);

    }

    /**
     * 拼凑积分变缓相关的信息
     *
     * @param validCountPO      有效单情况
     * @param userProfilePOList 个人信息变更列表
     * @param scoreRecordPOList 积分变更列表
     * @param scoreRecordBO     分数对象
     */
    private void getScoreChangeList(ValidCountPO validCountPO,
                                    List<UserProfilePO> userProfilePOList
            , List<ScoreRecordPO> scoreRecordPOList, ScoreRecordBO scoreRecordBO) {
        UserProfilePO updateUser = new UserProfilePO().setUserId(validCountPO.getUserId())
                .setTotalScore(scoreRecordBO.getScore());
        ScoreRecordPO scoreRecordPO = new ScoreRecordPO().setUserId(validCountPO.getUserId()).setParkingOrderId(0L)
                .setScore(scoreRecordBO.getScore()).setName(validCountPO.getName()).setJobNumber(validCountPO.getJobNumber()).setRemark(scoreRecordBO.getRemark());
        userProfilePOList.add(updateUser);
        scoreRecordPOList.add(scoreRecordPO);
    }

    /**
     * 根据乘车次数获取相对应的积分
     *
     * @param count 开车次数
     * @return 分数
     */
    private ScoreRecordBO getPassengerScoreByCount(Long count) {
        ScoreRecordBO scoreRecordBO = new ScoreRecordBO();
        if (count >= 30) {
            return scoreRecordBO.setScore(100).setRemark("搭车超过30单，奖励100积分");
        } else if (count >= 20) {
            return scoreRecordBO.setScore(75).setRemark("搭车超过20单，奖励75积分");
        } else if (count >= 10) {
            return scoreRecordBO.setScore(50).setRemark("搭车超过10单，奖励50积分");
        }
        return scoreRecordBO;
    }

    /**
     * 根据开车次数获取相对应的积分
     *
     * @param count 开车次数
     * @return 分数
     */
    private ScoreRecordBO getDriverScoreByCount(Long count) {
        ScoreRecordBO scoreRecordBO = new ScoreRecordBO();
        if (count >= 30) {
            return scoreRecordBO.setScore(300).setRemark("开车超过30单，奖励300积分");
        } else if (count >= 20) {
            return scoreRecordBO.setScore(200).setRemark("开车超过20单，奖励200积分");
        } else if (count >= 10) {
            return scoreRecordBO.setScore(100).setRemark("开车超过10单，奖励100积分");
        }
        return scoreRecordBO;
    }

    /**
     * 处理过期的订单
     *
     * @param parkingOrderDTOList 需要处理的订单
     */
    private void dealOutTimeOrder(List<ParkingOrderDTO> parkingOrderDTOList) {
        List<Long> needInstantlyOrderIdList = parkingOrderDTOList.stream().map(ParkingOrderDTO::getParkingOrderId).collect(Collectors.toList());

        if (CollectionUtils.isEmpty(needInstantlyOrderIdList)) {
            return;
        }
        //查询同行表确认和请求状态的同行单
        List<OrderPeerPO> orderPeerPOList = orderPeerService.
                getOrderPeerByOrderListAndStatusList(needInstantlyOrderIdList, Arrays.asList(OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode()
                        , OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode()));
        //取消超时订单
        cancelOutTimeOrder(parkingOrderDTOList, orderPeerPOList);
        //提醒15分钟之后要开始的订单
        remindOnTimeOrder(parkingOrderDTOList, orderPeerPOList);

    }

    /**
     * 提醒到时间的订单
     *
     * @param parkingOrderDTOList 需要处理的订单
     * @param orderPeerPOList     同行的订单
     */
    private void remindOnTimeOrder(List<ParkingOrderDTO> parkingOrderDTOList, List<OrderPeerPO> orderPeerPOList) {
        //过滤出需要提醒的订单
        parkingOrderDTOList = parkingOrderDTOList.stream()
                .filter(order -> ObjectUtils.isNotEmpty(order) && Objects.equals(OrderStateEnum.ORDER_ING.getCode(), order.getOrderState())).
                collect(Collectors.toList());
        if (CollectionUtils.isEmpty(parkingOrderDTOList)) {
            return;
        }
        //获取所有需要查询的用户id
        Set<Long> userIdSet = parkingOrderDTOList.stream().map(ParkingOrderDTO::getUserId).collect(Collectors.toSet());
        //需要通知的订单id
        Set<Long> needQueryOrderIdList = parkingOrderDTOList.stream().map(ParkingOrderDTO::getParkingOrderId).collect(Collectors.toSet());
        orderPeerPOList = orderPeerPOList.stream().filter(o -> needQueryOrderIdList.contains(o.getParkingOrderId())
                && Objects.equals(OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode(), o.getRecordState())).collect(Collectors.toList());
        //同行表里面的也需要查询
        userIdSet.addAll(orderPeerPOList.stream().map(OrderPeerPO::getUserId).collect(Collectors.toSet()));
        List<UserPO> userByUserIdList = userService.getUserByUserIdList(userIdSet);
        Map<Long, UserPO> userGroupByUserIdMap = userByUserIdList.stream()
                .collect(Collectors.toMap(UserPO::getUserId, userProfile -> userProfile));
        //超时通知
        List<TextMessageDTO> textMessageDTOList = new LinkedList<>();
        List<OrderPeerPO> finalOrderPeerPOList1 = orderPeerPOList;
        parkingOrderDTOList.forEach(order -> {
            //司机提醒
            UserPO userPO = userGroupByUserIdMap.get(order.getUserId());
            long outTime = DateUtil.between(new Date(), order.getOrderTime(), DateUnit.MINUTE);
            if (ObjectUtils.isNotEmpty(userPO)) {
                textMessageDTOList.add(new TextMessageDTO().setOpenIdList(Collections.singletonList(userPO.getOpenId()))
                        .setMessage(MessageConstant.getStartDriverOrderMessage(outTime, order.getOrderTime())));
            }
            finalOrderPeerPOList1.stream().filter
                    (peer -> Objects.equals(OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode(), peer.getRecordState())
                            && order.getParkingOrderId().equals(peer.getParkingOrderId())).forEach(peer -> {
                //乘客提醒
                UserPO peerUser = userGroupByUserIdMap.get(peer.getUserId());
                if (ObjectUtils.isNotEmpty(peerUser)) {
                    textMessageDTOList.add(new TextMessageDTO().setOpenIdList(Collections.singletonList(peerUser.getOpenId()))
                            .setMessage(MessageConstant.getStartPassengerOrderMessage(outTime, order.getOrderTime())));

                }
            });

        });
        //异步通知
        dingTalkMessageFacade.asyncSendBatchText(textMessageDTOList);
        //
        parkingOrderService.update(new LambdaUpdateWrapper<ParkingOrderPO>().in(ParkingOrderPO::getParkingOrderId, needQueryOrderIdList)
                .set(ParkingOrderPO::getNoticed, NoticedEnum.NOTICED.getCode()));
    }

    /**
     * 取消超时订单
     *
     * @param parkingOrderDTOList 订单
     * @param orderPeerPOList     同行订单
     */
    private void cancelOutTimeOrder(List<ParkingOrderDTO> parkingOrderDTOList, List<OrderPeerPO> orderPeerPOList) {
        //获取需要取消的订单id
        List<Long> needCancelOrderList = parkingOrderDTOList.stream()
                .filter(order -> Objects.equals(OrderStateEnum.ORDER_NOT_START.getCode(), order.getOrderState())).map(ParkingOrderDTO::getParkingOrderId)
                .collect(Collectors.toList());
        if (CollectionUtils.isEmpty(needCancelOrderList)) {
            return;
        }
        parkingOrderService.update(new LambdaUpdateWrapper<ParkingOrderPO>()
                .in(ParkingOrderPO::getParkingOrderId, needCancelOrderList)
                .eq(ParkingOrderPO::getOrderState, OrderStateEnum.ORDER_NOT_START.getCode())
                .set(ParkingOrderPO::getOrderState, OrderStateEnum.ORDER_CANCEL.getCode()));
        //获取需要取消的同行id
        List<Long> needCancelPeerList = orderPeerPOList.stream()
                .filter(peer -> needCancelOrderList.contains(peer.getParkingOrderId()))
                .map(OrderPeerPO::getOrderPeerId)
                .collect(Collectors.toList());
        //订单状态变化记录
        List<OrderStateRecordPO> orderStateRecordPOList = needCancelOrderList.stream().map(parkingOrderId -> {
            OrderStateRecordPO orderStateRecordPO = new OrderStateRecordPO();
            orderStateRecordPO.setParkingOrderId(parkingOrderId);
            orderStateRecordPO.setOptType(OptTypeEnum.CANCEL_ON_EXPIRE.getCode());
            orderStateRecordPO.setUserType(UserTypeEnum.AUTO.getCode());
            orderStateRecordPO.setUserId(0L);
            return orderStateRecordPO;
        }).collect(Collectors.toList());
        orderStateRecordService.saveBatch(orderStateRecordPOList);
        //将同行人释放
        if (CollectionUtils.isNotEmpty(needCancelPeerList)) {
            orderPeerService.update(new LambdaUpdateWrapper<OrderPeerPO>()
                    .in(OrderPeerPO::getOrderPeerId, needCancelPeerList)
                    .eq(OrderPeerPO::getRecordState, OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode())
                    .set(OrderPeerPO::getRecordState, OrderPeerStateEnum.ORDER_PEER_FAIL.getCode()));
        }
        log.info("自动取消完成，订单号是[{}]", needCancelOrderList.stream().map(String::valueOf).collect(Collectors.joining(",")));
    }

    /**
     * 处理完超时订单的评价
     *
     * @param parkingOrderPOList
     * @param orderPeerPOList
     */
    private void dealEvaluate(List<ParkingOrderPO> parkingOrderPOList, List<OrderPeerPO> orderPeerPOList) {
        List<ParkingEvaluatePO> evaluateList = new LinkedList<>();
        Map<Long, List<OrderPeerPO>> orderPeerById = orderPeerPOList.stream()
                .collect(Collectors.groupingBy(OrderPeerPO::getParkingOrderId));
        for (ParkingOrderPO parkingOrderPO : parkingOrderPOList) {
            List<OrderPeerPO> orderPeerPOS = orderPeerById.get(parkingOrderPO.getParkingOrderId());

            if (CollectionUtils.isNotEmpty(orderPeerPOS)) {
                for (OrderPeerPO orderPeerPO : orderPeerPOList) {
                    //司机
                    evaluateList.add(new ParkingEvaluatePO().setUserId(parkingOrderPO.getUserId()).setParkingOrderId(parkingOrderPO.getParkingOrderId())
                            .setEvaluateType(EvaluateTypeEnum.EVALUATE_PASSENGER.getCode()).setEvaluateUserId(orderPeerPO.getUserId()));
                    //乘客
                    evaluateList.add(new ParkingEvaluatePO().setUserId(orderPeerPO.getUserId()).setParkingOrderId(parkingOrderPO.getParkingOrderId())
                            .setEvaluateType(EvaluateTypeEnum.EVALUATE_DRIVER.getCode()).setEvaluateUserId(parkingOrderPO.getUserId()));


                }
            }
        }
        parkingEvaluateService.saveBatch(evaluateList);
    }
}
