package com.cf.parking.services.facade.impl;

import cn.hutool.core.date.DateUnit;
import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.dao.mapper.OrderStateRecordPOMapper;
import com.cf.parking.dao.mapper.ParkingOrderPOMapper;
import com.cf.parking.dao.po.*;
import com.cf.parking.facade.bo.*;
import com.cf.parking.facade.constant.MessageConstant;
import com.cf.parking.facade.dto.*;
import com.cf.parking.facade.enums.*;
import com.cf.parking.facade.facade.DingTalkMessageFacade;
import com.cf.parking.facade.facade.OrderDelayFacade;
import com.cf.parking.facade.facade.ParkingOrderFacade;
import com.cf.parking.services.job.order.OrderDelayJobUtil;
import com.cf.parking.services.properties.DingTalkProperties;
import com.cf.parking.services.service.*;
import com.cf.parking.services.utils.EmptyUtils;
import com.cf.parking.services.utils.LongLatitudeUtils;
import com.cf.parking.services.utils.PageUtils;
import com.cf.parking.services.utils.StateUtils;
import com.cf.support.bean.IdWorker;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.CFDateUtils;
import com.cf.support.utils.DingAlarmUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.apache.poi.ss.formula.functions.T;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;


/**
 * @author: lpy
 * @Date: 2022/10/20
 */
@Service
@Slf4j
public class ParkingOrderFacadeImpl implements ParkingOrderFacade {
    /**
     * 展示的最小距离
     */
    private final String MIN_DISTANCE_SHOW = "<500m";

    /**
     * 最小距离
     */
    private final Double MIN_DISTANCE = 0.5;

    /**
     * 展示的最大距离
     */
    private final String MAX_DISTANCE_SHOW = "<1km";

    /**
     * 展示的最大距离
     */
    private final Double MAX_DISTANCE = 1D;

    /**
     * 单位
     */
    private final String UNIT_MATHEMATICS = "km";

    /**
     * 乘客每一单积分
     */
    private final Integer PASSENGER_EACH_ORDER_SCORE = 5;
    /**
     * 司机每一单积分倍率
     */
    private final Integer DRIVER_EACH_ORDER_RATIO = 10;
    @Resource
    private ParkingOrderService parkingOrderService;
    @Resource
    private ParkingOrderPOMapper parkingOrderPOMapper;
    @Resource
    private OrderPeerService orderPeerService;
    @Resource
    private ScoreRecordService scoreRecordService;
    @Resource
    private IdWorker idWorker;

    @Resource
    private UserProfileService userProfileService;

    @Resource
    private OrderStateRecordService orderStateRecordService;
    @Resource
    private OrderStateRecordPOMapper orderStateRecordPOMapper;
    @Resource
    private ParkingEvaluateService parkingEvaluateService;

    @Resource
    private OrderDelayFacade orderDelayFacade;

    @Resource
    private DingTalkMessageFacade dingTalkMessageFacade;
    @Resource
    private DingTalkProperties dingTalkProperties;

    /**
     * 订单提交
     *
     * @param param
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<OrderCommitBO> commitOrder(ParkingOrderDTO param) {
        Long userId = param.getUserId();
        Date orderTime = param.getOrderTime();
        ParkingOrderPO parkingOrderPO = new ParkingOrderPO().setUserId(userId).setOrderTime(orderTime);
        List<ParkingOrderPO> parkingOrderPOS = parkingOrderService.getOrderListByUserIdAndOrderTime(parkingOrderPO).stream().filter(item ->
                DateFormatUtils.format(orderTime, "yyyy-MM-dd").equals(DateFormatUtils.format(item.getOrderTime(), "yyyy-MM-dd"))
        ).collect(Collectors.toList());
        // 获取当天12点
        Date twelveClockTime = this.getSpecifiedTime(orderTime, 12);
        // 司机限制发布两单
        this.checkOrderCommitTimes(parkingOrderPOS, orderTime, twelveClockTime);
        // 查询profile表
        UserProfilePO driver = userProfileService.getById(userId);
        // 检测是否有用户
        EmptyUtils.checkProfile(driver, userId, "提交订单");
        // 发布订单，车牌不能为空
        if (ObjectUtils.isEmpty(driver.getPlateNo())) {
            throw new BusinessException(BizResultCodeEnum.PLATE_NO_IS_NULL);
        }
        Long parkingOrderId = idWorker.nextId();
        BeanConvertorUtils.copy(param, parkingOrderPO);
        Double distance = LongLatitudeUtils.getDistance(param.getStartLongitude(), param.getStartLatitude(), param.getDestLongitude(), param.getDestLatitude());
        parkingOrderPO.setParkingOrderId(parkingOrderId).setJobNumber(driver.getJobNumber()).setName(driver.getName())
                .setNoticed(NoticedEnum.NOT_NOTICED.getCode()).setOrderState(OrderStateEnum.ORDER_NOT_START.getCode()).setDistance(BigDecimal.valueOf(distance));
        // 插入订单表
        parkingOrderPOMapper.insert(parkingOrderPO);

        // 插入订单状态变更表
        OrderStateRecordPO orderStateRecordPO = new OrderStateRecordPO().setParkingOrderId(parkingOrderId)
                .setUserType(UserTypeEnum.DRIVER.getCode()).setUserId(userId).setOptType(OptTypeEnum.DRIVER_COMMIT_ORDER.getCode());
        orderStateRecordPOMapper.insertSelective(orderStateRecordPO);

        return Result.buildSuccessResult(new OrderCommitBO().setParkingOrderId(parkingOrderId));
    }

    /**
     * 订单详情
     *
     * @param userId
     * @param parkingOrderId
     * @return
     */
    @Override
    public Result<OrderDetailBO> orderDetail(Long userId, Long parkingOrderId) {
        ParkingOrderPO parkingOrderPO = parkingOrderService.getById(parkingOrderId);
        EmptyUtils.checkOrder(parkingOrderPO, parkingOrderId, "订单详情");
        Long driverId = parkingOrderPO.getUserId();
        Integer orderState = parkingOrderPO.getOrderState();
        //订单详情
        OrderBO orderBO = BeanConvertorUtils.map(parkingOrderPO, OrderBO.class);
        orderBO.setOrderTimeShow(DateFormatUtils.format(orderBO.getOrderTime(), "M月d日 HH:mm"));
        OrderDetailBO orderDetail = new OrderDetailBO().setOrder(orderBO);
        //订单状态为：未开始
        if (orderState.equals(OrderStateEnum.ORDER_NOT_START.getCode())) {
            //未开始状态，只有司机才能看到订单详情
            if (!userId.equals(driverId)) {
                //恶意调接口
                return Result.buildResult(BizResultCodeEnum.USER_ERROR);
            }
            //前端展示司机取消订单按钮，再来一单按钮
            orderDetail.setCancelType(CancelTypeEnum.CANCEL_BY_DRIVER.getCode());
            orderDetail.setUserType(UserTypeEnum.DRIVER.getCode());

            //目前只有未开始状态才返回同行记录，下期支持多个确认同行，需要加上进行中状态
            List<OrderPeerPO> orderPeerPOS = orderPeerService.getRequestedList(parkingOrderId);
            //  list每条根据userId 查询profile表获取avatar
            List<Long> userIdList = orderPeerPOS.stream().map(OrderPeerPO::getUserId).collect(Collectors.toList());
            if (CollectionUtils.isEmpty(userIdList)) {
                orderDetail.setList(new ArrayList<>());
                return Result.buildSuccessResult(orderDetail);
            }
            List<UserProfilePO> userProfilePOS = userProfileService.listByIds(userIdList);
            List<PassengerPeerBO> passengerPeerBOS = this.getPassengerBOWithAvatar(userProfilePOS, orderPeerPOS);
            orderDetail.setList(passengerPeerBOS);
            return Result.buildSuccessResult(orderDetail);
        }

        OrderPeerPO orderPeerPO = orderPeerService.getOrderPeerByState(parkingOrderId);
        Long passengerUserId = ObjectUtils.isNotEmpty(orderPeerPO) ? orderPeerPO.getUserId() : 0L;
        if (userId.equals(driverId)) {    //用户为司机
            orderDetail.setUserType(UserTypeEnum.DRIVER.getCode());
            if (ObjectUtils.isEmpty(orderPeerPO)) {
                return Result.buildSuccessResult(orderDetail);
            }
            UserBO userBO = BeanConvertorUtils.map(userProfileService.getById(passengerUserId), UserBO.class);
            userBO.setTitle(TitleTypeEnum.PASSENGER_TITLE.getMsg());
            orderDetail.setProfile(userBO);
            if (OrderStateEnum.ORDER_ING.getCode().equals(orderState)) {
                //进行中状态，司机可以取消订单
                orderDetail.setCancelType(CancelTypeEnum.CANCEL_BY_DRIVER.getCode());
            }
            return Result.buildSuccessResult(orderDetail);
        } else if (userId.equals(passengerUserId)) {   //用户为乘客
            UserBO userBO = BeanConvertorUtils.map(userProfileService.getById(driverId), UserBO.class);
            userBO.setTitle(TitleTypeEnum.DRIVER_TITLE.getMsg());
            orderDetail.setProfile(userBO);
            orderDetail.setUserType(UserTypeEnum.PASSENGER.getCode());
            if (OrderStateEnum.ORDER_ING.getCode().equals(orderState)) {
                //进行中状态，乘客可以取消订单
                orderDetail.setCancelType(CancelTypeEnum.CANCEL_BY_PASSENGER.getCode());

                if (OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode().equals(orderPeerPO.getRecordState())) {
                    //订单进行中状态,同行状态为司机确认后，乘客显示确认上车按钮
                    orderDetail.setOptType(OrderOptTypeEnum.CONFIRM_BOARDING.getCode());
                } else if (OrderPeerStateEnum.ORDER_PEER_CONFIRM_RIDING.getCode().equals(orderPeerPO.getRecordState())) {
                    //订单进行中状态,同行状态为乘客确认上车后，乘客显示到达目的地按钮
                    orderDetail.setOptType(OrderOptTypeEnum.ARRIVE_BY_PASSENGER.getCode());
                }
            }
            return Result.buildSuccessResult(orderDetail);
        } else {
            //恶意调接口
            return Result.buildResult(BizResultCodeEnum.USER_ERROR);
        }
    }

    /**
     * 司机未完成订单
     *
     * @param userId
     * @return
     */
    @Override
    public List<DriverOrderUnfinishedBO> pendingDrive(Long userId) {
        // order_state== 1 || order_state== 2 (状态为 未开始 或 进行中 )，
        // 过滤掉已过期的订单（主要是未开始订单）
        List<ParkingOrderPO> list = parkingOrderService.getListByUserIdAndState(userId);
        if (CollectionUtils.isEmpty(list)) {
            return new ArrayList<>();
        }
        return list.stream()
                .filter(o -> !(OrderStateEnum.ORDER_NOT_START.getCode()
                        .equals(o.getOrderState()) && new Date().after(o.getOrderTime())))
                .map(item -> {
                    String startAddress = item.getStartCounty() + item.getStartAddress();
                    String destAddress = item.getDestCounty() + item.getDestAddress();
                    DriverOrderUnfinishedBO driverOrderUnfinishedBO = new DriverOrderUnfinishedBO()
                            .setParkingOrderId(item.getParkingOrderId())
                            .setStartAddress(startAddress)
                            .setDestAddress(destAddress)
                            .setOrderTimeShow(CFDateUtils.getDateShow(item.getOrderTime()))
                            // 未开始:正在寻找乘客  进行中:正在前往目的地    正在前往目的地  改成两个状态， 出发时间离当天很近，  很远   半个小时内
                            .setOrderStateMsg(item.getOrderState().equals(OrderStateEnum.ORDER_NOT_START.getCode()) ? "正在寻找乘客" : DateUtil.isIn(new Date(), DateUtil.offsetMinute(item.getOrderTime(), -30), item.getOrderTime()) ? "正在前往目的地" : "即将出发")
                            .setOrderTime(item.getOrderTime());
                    return driverOrderUnfinishedBO;
                }).limit(2).collect(Collectors.toList());
    }

    /**
     * 司机确认同行
     *
     * @param orderConfirmPeerDTO
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result orderConfirmPeer(OrderConfirmPeerDTO orderConfirmPeerDTO) {
        Long userId = orderConfirmPeerDTO.getUserId();
        Long parkingOrderId = orderConfirmPeerDTO.getParkingOrderId();
        Long orderPeerId = orderConfirmPeerDTO.getOrderPeerId();

        ParkingOrderPO driver = parkingOrderService.getOrderByOrderId(parkingOrderId);
        OrderPeerPO byId = orderPeerService.getOrderPeerByPeerId(orderPeerId);
        // 权限校验
        this.checkPermission(userId, parkingOrderId, driver, byId);
        // 判断出发时间之后，，司机点击确认同行，页面需要错误提示 并且取消订单
        if (driver.getOrderTime().before(new Date())) {
            cancelOrder(userId, parkingOrderId, driver, OptTypeEnum.CANCEL_ON_EXPIRE.getCode(), UserTypeEnum.AUTO.getCode());
            // 这里不能抛异常回滚，就是要他成功的，
            return Result.buildResult(BizResultCodeEnum.ORDER_TIME_BEFORE_NOW);
        }

        //同行表的当前人记录状态改成 已确认
        if (!orderPeerService.updateOrderPeerStateByPeerId(orderPeerId)) {
            // 更新失败 ，抛出异常,
            throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH.getMsg());
        }

        // 同行表的其他人记录状态改成 未成单
        orderPeerService.updateOtherPeerStateByPeerId(orderPeerId, parkingOrderId);
        // 订单表状态改成 进行中
        ParkingOrderPO orderOnGoing = parkingOrderService.getOrderByOrderId(parkingOrderId);
        orderOnGoing.setOrderState(OrderStateEnum.ORDER_ING.getCode());
        if (!parkingOrderService.updateOrderStateByOrderId(parkingOrderId, OrderStateEnum.ORDER_ING.getCode())) {
            throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH.getMsg());
        }

        orderStateRecordPOMapper.insertSelective(new OrderStateRecordPO().setUserId(userId).setParkingOrderId(parkingOrderId).setOptType(OptTypeEnum.CONFIRM_PEER.getCode()).setUserType(UserTypeEnum.DRIVER.getCode()));
        this.passRequestMessage(byId.getParkingOrderId(), driver.getUserId(), byId.getUserId());
        return Result.buildSuccessResult();
    }

    /**
     * 司机取消订单
     *
     * @param userId
     * @param parkingOrderId
     * @return
     */
    @Override
    @Transactional
    public Result cancelDriver(Long userId, Long parkingOrderId) {
        ParkingOrderPO order = parkingOrderService.getOrderByOrderId(parkingOrderId);
        // 取消订单权限校验
        this.cancelDriverCheckPermission(order, userId, parkingOrderId);

        // 取消操作
        List<OrderPeerPO> list = cancelOrder(userId, parkingOrderId, order, OptTypeEnum.DRIVER_CANCEL.getCode(), UserTypeEnum.DRIVER.getCode());

        // 司机取消订单，发送给乘客通知
        if (CollectionUtils.isNotEmpty(list)) {
            Optional<OrderPeerPO> orderPeerPOOptional = list.stream().filter(orderPeerPO ->
                    OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode().equals(orderPeerPO.getRecordState())).findAny();
            if (orderPeerPOOptional.isPresent()) {
                OrderPeerPO orderPeerPO = orderPeerPOOptional.get();
                driverCancelMessage(order.getParkingOrderId(), orderPeerPO.getUserId(), userId, order.getOrderTime());
            }
        }
        return Result.buildSuccessResult();
    }

    public List<OrderPeerPO> cancelOrder(Long userId, Long parkingOrderId, ParkingOrderPO order, Integer cancelType, Integer cancelOperator) {
        List<OrderPeerPO> list = new LinkedList<>();
        // 同行表的该订单的所有记录 状态 = 未开始 改成 未成单 , 状态 = 进行中 改为 已取消
        if (order.getOrderState().equals(OrderStateEnum.ORDER_NOT_START.getCode())) {
            // 不用管成功
            orderPeerService.orderFail(parkingOrderId);
        } else {
            list = orderPeerService.getOrderPeerByOrderListAndStatusList(Collections.singletonList(parkingOrderId), new ArrayList<>());
            // 新增乘客确认上车
            List<Long> peerRecordIdList = list.stream()
                    .filter(item -> (item.getRecordState()
                            .equals(OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode()) || (OrderPeerStateEnum.ORDER_PEER_CONFIRM_RIDING.getCode().equals(item.getRecordState()))))
                    .map(OrderPeerPO::getOrderPeerId)
                    .collect(Collectors.toList());
            // 如果id列表为空 || (id列表不为空且更新失败）
            if (CollectionUtils.isEmpty(peerRecordIdList) || (CollectionUtils.isNotEmpty(peerRecordIdList) && !orderPeerService.updateOrderPeerByOrderPeerIdList(peerRecordIdList, OrderPeerStateEnum.ORDER_PEER_CANCEL.getCode()))) {
                throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH.getMsg());
            }
            // 取消操作上车时间检查
            EmptyUtils.cancelTimeCheck(list);
        }

        // 插入订单状态变更记录表
        OrderStateRecordPO orderStateRecordPO = new OrderStateRecordPO().setUserId(userId).setUserType(cancelOperator).setParkingOrderId(parkingOrderId).setOptType(cancelType);
        if (orderStateRecordPOMapper.insertSelective(orderStateRecordPO) < 1) {
            throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH.getMsg());
        }
        // 更新订单表
        if (!parkingOrderService.updateOrderStateByOrderId(parkingOrderId, OrderStateEnum.ORDER_CANCEL.getCode())) {
            throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH.getMsg());
        }
        return list;
    }

    /**
     * 获取我的订单记录
     *
     * @param orderRecordDTO
     * @return
     */
    @Override
    public PageResponse<OrderRecordBO> orderRecord(OrderRecordDTO orderRecordDTO) {
        Long userId = orderRecordDTO.getUserId();
        Integer userType = orderRecordDTO.getUserType();
        // 通过用户id查询用户信息表，获得开搭车次数与评分
        UserProfilePO userProfile = userProfileService.getById(userId);
        // 用户不存在
        EmptyUtils.checkProfile(userProfile, userId, "查询订单记录");
        this.orderRecordCheckPermission(userType);

        // 查询订单信息
        Page<T> page = PageUtils.toPage(orderRecordDTO);
        IPage<ParkingOrderPO> pageList = this.getPageList(userId, userType, page);

        // 没有订单
        if (ObjectUtils.isEmpty(pageList) || CollectionUtils.isEmpty(pageList.getRecords())) {
            return PageUtils.emptyResponseList(page);
        }

        List<OrderRecordBO> collect = this.getOrderRecordCollect(orderRecordDTO, pageList.getRecords());
        return PageUtils.toResponseList(pageList, collect);
    }

    @Override
    public PageResponse<ParkingOrderBO> getOrderPage(Long userId, RidingOrderDTO param) {
        // todo: parking order po类优化
        QueryPO queryPO = BeanConvertorUtils.map(param, QueryPO.class);
        queryPO.setOrderState(OrderStateEnum.ORDER_NOT_START.getCode());
        Page<T> page = new Page().setCurrent(param.getPageNo()).setSize(param.getPageSize());
        IPage selectPage = parkingOrderPOMapper.getOrderPage(page, queryPO);
        if (selectPage.getTotal() == 0) {
            return PageUtils.emptyResponseList(selectPage);
        }
        List<ParkingOrderPO> records = selectPage.getRecords();
        List<ParkingOrderBO> parkingOrderBOS = BeanConvertorUtils.copyList(records, ParkingOrderBO.class);
        //查询司机头像,评分
        List<Long> driverIds = records.stream().map(ParkingOrderPO::getUserId).collect(Collectors.toList());
        List<UserProfilePO> driversProfile = userProfileService.selectList(driverIds);
        Map<Long, UserProfilePO> driversMap = driversProfile.stream().collect(Collectors.toMap(UserProfilePO::getUserId, userProfilePO -> userProfilePO));
        //查询乘客是否已发过请求
        List<Long> orderIdList = orderPeerService.selectRequestOrderList(userId);
        for (ParkingOrderBO parkingOrderBO : parkingOrderBOS) {
            UserProfilePO profilePO = driversMap.get(parkingOrderBO.getUserId());
            Integer driverMark = profilePO.getDriveMark();
            if (driverMark == 0) {
                driverMark = 5;
            }
            parkingOrderBO.setOrderTimeShow(CFDateUtils.getDateShow(parkingOrderBO.getOrderTime()));
            parkingOrderBO.setAvatar(profilePO.getAvatar());
            parkingOrderBO.setDriveMark(driverMark);
            //已请求requestFlag置2
            parkingOrderBO.setRequestFlag(RequestFlagEnum.REQUEST_FLAG_NO.getCode());
            if (orderIdList.contains(parkingOrderBO.getParkingOrderId())) {
                parkingOrderBO.setRequestFlag(RequestFlagEnum.REQUEST_FLAG_SENT.getCode());
            }
            // 初始空字符串， 方便前端处理
            parkingOrderBO.setDestDistanceShow("").setStartDistanceShow("");
            if (ObjectUtils.isNotEmpty(parkingOrderBO.getStartDistance())) {
                Double startDis = parkingOrderBO.getStartDistance().divide(BigDecimal.valueOf(1000)).doubleValue();
                parkingOrderBO.setStartDistanceShow(this.showDistance(startDis));
            }
            if (ObjectUtils.isNotEmpty(parkingOrderBO.getDestDistance())) {
                Double destDis = parkingOrderBO.getDestDistance().divide(BigDecimal.valueOf(1000)).doubleValue();
                parkingOrderBO.setDestDistanceShow(this.showDistance(destDis));
            }

        }
        return PageUtils.toResponseList(selectPage, parkingOrderBOS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result requestConfirmOrder(Long userId, Long parkingOrderId) {
        ParkingOrderPO parkingOrderPO = parkingOrderService.getNotEmptyOrder(userId, parkingOrderId);
        //订单相关校验
        this.requestOrderCheck(parkingOrderPO, userId);
        //出发时间过期，进行过期操作
        Date orderTime = parkingOrderPO.getOrderTime();
        if (new Date().compareTo(orderTime) >= 0) {
            return expireOrder(parkingOrderId);
        }
        //同行记录相关校验
        OrderPeerPO flagPO = this.requestPeerCheck(parkingOrderPO, userId);
        //用户是否请求过该订单
        if (flagPO != null) {
            //查询状态=2(取消请求)，更新状态=1(已请求)
            OrderPeerPO peerPO = new OrderPeerPO().setOrderPeerId(flagPO.getOrderPeerId()).setRecordState(OrderPeerStateEnum.ORDER_PEER_CANCEL_REQUEST.getCode());
            orderPeerService.updateState(peerPO, OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode());
            hitchhikeRequestMessage(parkingOrderId, parkingOrderPO.getUserId(), flagPO.getUserId());
            return Result.buildSuccessResult();
        }
        //查询用户详情表,生成一条同行记录
        UserProfilePO profileByUserId = userProfileService.getById(userId);
        OrderPeerPO orderPeerPO = BeanConvertorUtils.map(profileByUserId, OrderPeerPO.class);
        orderPeerPO.setParkingOrderId(parkingOrderId).setOrderTime(orderTime);
        orderPeerService.insertOrderPeer(orderPeerPO);
        hitchhikeRequestMessage(parkingOrderId, parkingOrderPO.getUserId(), profileByUserId.getUserId());
        return Result.buildSuccessResult();
    }

    /**
     * 请求接口订单校验
     *
     * @param parkingOrderPO
     * @param userId
     */
    private void requestOrderCheck(ParkingOrderPO parkingOrderPO, Long userId) {
        StateUtils.orderStateCheck(parkingOrderPO.getOrderState(), OrderStateEnum.ORDER_NOT_START.getCode());
        //司机不能请求自己的订单
        if (parkingOrderPO.getUserId().equals(userId)) {
            throw new BusinessException(BizResultCodeEnum.ORDER_NOT_SAME.getMsg());
        }
        //一个订单限制请求次数(5)
        if (orderPeerService.countRecord(parkingOrderPO.getParkingOrderId()) >= 5L) {
            throw new BusinessException(BizResultCodeEnum.MAXIMUM_REQUESTS.getMsg());
        }
    }

    /**
     * 订单过期操作
     *
     * @param parkingOrderId
     * @return
     */
    private Result expireOrder(Long parkingOrderId) {
        // 更新同行记录状态(已请求->取消订单)
        OrderPeerPO peerPO = new OrderPeerPO().setParkingOrderId(parkingOrderId).setRecordState(OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode());
        if (ObjectUtils.isNotEmpty(peerPO.getUserId())) {
            orderPeerService.updateState(peerPO, OrderPeerStateEnum.ORDER_PEER_CANCEL.getCode());
        }
        //更新订单状态(未开始->已取消)
        ParkingOrderPO orderPO = new ParkingOrderPO().setParkingOrderId(parkingOrderId).setOrderState(OrderStateEnum.ORDER_NOT_START.getCode());
        parkingOrderService.updateOrder(orderPO, OrderStateEnum.ORDER_CANCEL.getCode());
        //订单状态变化记录
        OrderStateRecordPO orderStateRecordPO = new OrderStateRecordPO().setParkingOrderId(parkingOrderId).setOptType(OptTypeEnum.CANCEL_ON_EXPIRE.getCode());
        orderStateRecordService.saveOrderStateRecord(orderStateRecordPO);
        return Result.buildErrorResult(BizResultCodeEnum.ORDER_BE_OVERDUE.getMsg());
    }

    /**
     * 请求接口同行记录校验
     *
     * @param parkingOrderPO
     * @param userId
     * @return
     */
    private OrderPeerPO requestPeerCheck(ParkingOrderPO parkingOrderPO, Long userId) {
        //查询同行记录
        int count = 0;
        OrderPeerPO flagPO = null;
        Date orderTime = parkingOrderPO.getOrderTime();
        Integer flagTime = CFDateUtils.getFlagTime(orderTime);
        String orderDate = DateFormatUtils.format(orderTime, "yyyy-MM-dd");
        List<Integer> stateList = Arrays.asList(OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode(), OrderPeerStateEnum.ORDER_PEER_COMPLETE.getCode());
        List<OrderPeerPO> recordByUserId = orderPeerService.selectRecordByUserId(userId);
        for (OrderPeerPO peerPO : recordByUserId) {
            Integer recordState = peerPO.getRecordState();
            Date peerTime = peerPO.getOrderTime();
            //是否存在有效订单,上午一单，下午一单
            if (DateFormatUtils.format(peerTime, "yyyy-MM-dd").equals(orderDate) && stateList.contains(recordState) && flagTime.equals(CFDateUtils.getFlagTime(peerTime))) {
                throw new BusinessException(BizResultCodeEnum.ORDER_REQUEST_CONFIRM.getMsg());
            }
            //已请求次数
            if (recordState.equals(OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode())) {
                count++;
            }
            //同行表中有该订单相关记录
            if (peerPO.getParkingOrderId().equals(parkingOrderPO.getParkingOrderId())) {
                if (recordState.equals(OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode())) {
                    throw new BusinessException(BizResultCodeEnum.ORDER_ALREADY_REQUEST.getMsg());
                }
                if (recordState.equals(OrderPeerStateEnum.ORDER_PEER_CANCEL_REQUEST.getCode())) {
                    flagPO = peerPO;
                }
            }
        }
        //超出最大请求数
        if (count >= 3) {
            throw new BusinessException(BizResultCodeEnum.ORDER_REQUEST_LIMIT.getMsg());
        }
        return flagPO;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result requestCancelOrder(Long userId, Long parkingOrderId) {
        ParkingOrderPO parkingOrderPO = parkingOrderService.getNotEmptyOrder(userId, parkingOrderId);
        StateUtils.orderStateCheck(parkingOrderPO.getOrderState(), OrderStateEnum.ORDER_NOT_START.getCode());
        //查询同行记录,校验状态
        OrderPeerPO orderPeerPO = orderPeerService.selectOne(parkingOrderId, userId);
        StateUtils.recordStateCheck(orderPeerPO.getRecordState(), OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode());
        //更新状态为record_state=2(取消请求)
        OrderPeerPO peerPO = new OrderPeerPO().setOrderPeerId(orderPeerPO.getOrderPeerId()).setRecordState(OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode());
        orderPeerService.updateState(peerPO, OrderPeerStateEnum.ORDER_PEER_CANCEL_REQUEST.getCode());
        return Result.buildSuccessResult();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result cancelByPassenger(Long userId, Long parkingOrderId) {
        ParkingOrderPO parkingOrderPO = parkingOrderService.getNotEmptyOrder(userId, parkingOrderId);
        //订单校验更新
        cancelOrderCheckAndUpdate(parkingOrderPO, userId);
        //同行记录校验更新
        cancelPeerCheckAndUpdate(parkingOrderId, userId);
        this.hitchhikeCancelMessage(parkingOrderPO.getParkingOrderId(), parkingOrderPO.getUserId(), userId, parkingOrderPO.getOrderTime());
        return Result.buildSuccessResult();
    }

    /**
     * 乘客取消订单接口订单校验更新
     *
     * @param parkingOrderPO
     * @param userId
     */
    private void cancelOrderCheckAndUpdate(ParkingOrderPO parkingOrderPO, Long userId) {
        StateUtils.orderStateCheck(parkingOrderPO.getOrderState(), OrderStateEnum.ORDER_ING.getCode());
        Long parkingOrderId = parkingOrderPO.getParkingOrderId();
        //更新订单状态为order_state=3(已取消)
        ParkingOrderPO orderPO = new ParkingOrderPO().setParkingOrderId(parkingOrderId).setOrderState(OrderStateEnum.ORDER_ING.getCode());
        parkingOrderService.updateOrder(orderPO, OrderStateEnum.ORDER_CANCEL.getCode());
        //插入一条订单状态变更记录
        OrderStateRecordPO orderStateRecordPO = new OrderStateRecordPO().setParkingOrderId(parkingOrderId)
                .setUserType(UserTypeEnum.PASSENGER.getCode()).setUserId(userId).setOptType(OptTypeEnum.PASSENGER_CANCEL.getCode());
        orderStateRecordService.saveOrderStateRecord(orderStateRecordPO);
    }

    /**
     * 乘客取消订单接口同行记录校验更新
     *
     * @param parkingOrderId
     * @param userId
     */
    private void cancelPeerCheckAndUpdate(Long parkingOrderId, Long userId) {
        //查询同行记录,校验状态record_state=4(已确定)
        OrderPeerPO orderPeerPO = orderPeerService.selectOne(parkingOrderId, userId);
        StateUtils.recordStateListCheck(orderPeerPO.getRecordState(), Arrays.asList(OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode(), OrderPeerStateEnum.ORDER_PEER_CONFIRM_RIDING.getCode()));
        // 确认上车十分钟内可以取消
        EmptyUtils.cancelTimeCheck(Arrays.asList(orderPeerPO));
        //更新同行状态为record_state=5(取消订单)
        OrderPeerPO peerPO = new OrderPeerPO().setOrderPeerId(orderPeerPO.getOrderPeerId()).setRecordState(OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode());
        orderPeerService.updateStateList(peerPO, OrderPeerStateEnum.ORDER_PEER_CANCEL.getCode(), Arrays.asList(OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode(), OrderPeerStateEnum.ORDER_PEER_CONFIRM_RIDING.getCode()));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result arriveByPassenger(Long userId, PassengerArriveDTO param) {
        Long parkingOrderId = param.getParkingOrderId();
        ParkingOrderPO parkingOrderPO = parkingOrderService.getNotEmptyOrder(userId, parkingOrderId);
        //订单校验更新
        this.arriveOrderCheckAndUpdate(parkingOrderPO, userId);
        // 乘客到达地距离司机发单结束地距离
        Double destDistance = LongLatitudeUtils.getDistance(param.getPassengerDestLongitude(), param.getPassengerDestLatitude(), parkingOrderPO.getDestLongitude(), parkingOrderPO.getDestLatitude());
        //同行记录校验更新 return: 乘客上车点下车点距离
        Double startDestDistance = arrivePeerCheckAndUpdate(parkingOrderId, userId, destDistance, param);
        // 生成评价记录2条（乘客，司机）
        parkingEvaluateService.saveEvaluateRecord(parkingOrderPO, userId);
        //司机开车次数+1，乘客搭车次数+1
        log.info("乘客确认下车 distance: 乘客 userId:{}, orderId:{},  距离目的地点距离：{}, 乘客上车点下车点距离:{}", userId, parkingOrderId, destDistance, startDestDistance);
        Integer score = parkingOrderPO.getDistance().multiply(BigDecimal.valueOf(DRIVER_EACH_ORDER_RATIO)).intValue();
        List<UserProfilePO> userProfilePOList = Arrays.asList(new UserProfilePO().setDriveTime(1).setUserId(parkingOrderPO.getUserId()).setDriveTotalKilometers(parkingOrderPO.getDistance()).setTotalScore(score), new UserProfilePO().setRideTime(1).setUserId(userId).setRideTotalKilometers(BigDecimal.valueOf(startDestDistance)).setTotalScore(PASSENGER_EACH_ORDER_SCORE));
        UserProfilePO driverProfile = userProfileService.getUserProfileByUserId(parkingOrderPO.getUserId());
        UserProfilePO passengerProfile = userProfileService.getUserProfileByUserId(userId);
        EmptyUtils.checkProfile(passengerProfile, userId, "确认下车");
        EmptyUtils.checkProfile(driverProfile, parkingOrderPO.getUserId(), "确认下车");
        List<ScoreRecordPO> scoreRecordPOS = Arrays.asList(new ScoreRecordPO().setParkingOrderId(parkingOrderId).setUserId(parkingOrderPO.getUserId()).setName(driverProfile.getName()).setJobNumber(driverProfile.getJobNumber()).setScore(score), new ScoreRecordPO().setParkingOrderId(parkingOrderId).setUserId(userId).setName(passengerProfile.getName()).setJobNumber(passengerProfile.getJobNumber()).setScore(PASSENGER_EACH_ORDER_SCORE));
        scoreRecordService.saveBatch(scoreRecordPOS);
        userProfileService.updateSelectionList(userProfilePOList);
        return Result.buildSuccessResult();
    }

    /**
     * 到达接口订单校验更新
     *
     * @param parkingOrderPO
     * @param userId
     */
    private void arriveOrderCheckAndUpdate(ParkingOrderPO parkingOrderPO, Long userId) {
        StateUtils.orderStateCheck(parkingOrderPO.getOrderState(), OrderStateEnum.ORDER_ING.getCode());
        Long parkingOrderId = parkingOrderPO.getParkingOrderId();
        //更新订单状态为order_state=4(已完成)
        ParkingOrderPO orderPO = new ParkingOrderPO().setParkingOrderId(parkingOrderId).setOrderState(OrderStateEnum.ORDER_ING.getCode());
        parkingOrderService.updateOrder(orderPO, OrderStateEnum.ORDER_COMPLETE.getCode());
        //插入一条订单状态变更记录
        OrderStateRecordPO orderStateRecordPO = new OrderStateRecordPO().setParkingOrderId(parkingOrderId)
                .setUserType(UserTypeEnum.PASSENGER.getCode()).setUserId(userId).setOptType(OptTypeEnum.PASSENGER_ARRIVE.getCode());
        orderStateRecordService.saveOrderStateRecord(orderStateRecordPO);
    }

    /**
     * 到达接口同行记录校验更新
     *
     * @param parkingOrderId
     * @param userId
     * @return
     */
    private Double arrivePeerCheckAndUpdate(Long parkingOrderId, Long userId, Double destDistance, PassengerArriveDTO param) {
        //查询同行记录,校验状态record_state=4(已确定)
        OrderPeerPO orderPeerPO = orderPeerService.selectOne(parkingOrderId, userId);
        // 乘客已确认上车  旧逻辑修改  新增
        StateUtils.recordStateCheck(orderPeerPO.getRecordState(), OrderPeerStateEnum.ORDER_PEER_CONFIRM_RIDING.getCode());
        // 乘客上车点下车点距离
        Double startDestDist = LongLatitudeUtils.getDistance(orderPeerPO.getPassengerStartLongitude(), orderPeerPO.getPassengerStartLatitude(), param.getPassengerDestLongitude(), param.getPassengerDestLatitude());
        //更新同行状态为record_state=6(已结束)
        // 旧逻辑修改 新增
        OrderPeerPO peerPO = new OrderPeerPO().setOrderPeerId(orderPeerPO.getOrderPeerId()).setRecordState(OrderPeerStateEnum.ORDER_PEER_CONFIRM_RIDING.getCode()).setDestDistance(BigDecimal.valueOf(destDistance)).setStartDestDistance(BigDecimal.valueOf(startDestDist)).setPassengerDestLongitude(param.getPassengerDestLongitude()).setPassengerDestLatitude(param.getPassengerDestLatitude()).setGetOffTm(new Date());
        orderPeerService.updateStateLongLatAndDis(peerPO, OrderPeerStateEnum.ORDER_PEER_COMPLETE.getCode());
        return startDestDist;
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void dealOutTimeAndStartOrder() {
        List<ParkingOrderPO> parkingOrderPOList = parkingOrderService.getCancelAndStartOrder(25);
        if (CollectionUtils.isNotEmpty(parkingOrderPOList)) {
            //1.将即将要发送超时消息的orderId放入set中
            //2.根据要启动的时长分类，统一放入一个线程中执行
            Map<Long, List<OrderTimeTaskDTO>> groupByRunMinute = parkingOrderPOList.stream()
                    //过滤掉已经扫描过的订单
                    .filter(orderPO -> !OrderDelayJobUtil.needNoticeOrderId.contains(orderPO.getParkingOrderId())
                            &&(OrderStateEnum.ORDER_NOT_START.getCode().equals(orderPO.getOrderState())
                            ||DateUtil.between(new Date(), orderPO.getOrderTime(), DateUnit.MINUTE) > 15) )
                    .map(orderPO -> {
                        OrderDelayJobUtil.needNoticeOrderId.add(orderPO.getParkingOrderId());
                        return new OrderTimeTaskDTO().setParkingOrderId(orderPO.getParkingOrderId())
                                .setRunMinute(Objects.equals(OrderStateEnum.ORDER_NOT_START.getCode(), orderPO.getOrderState()) ?
                                        DateUtil.between(new Date(), orderPO.getOrderTime(),
                                                DateUnit.MINUTE) : DateUtil.between(new Date(), orderPO.getOrderTime(), DateUnit.MINUTE) - 15);
                    }).collect(Collectors.groupingBy(OrderTimeTaskDTO::getRunMinute));
            groupByRunMinute.forEach((runMinute, list) -> {
                //放入任务处理
                OrderDelayJobUtil.scheduledExecutorService.schedule(() -> {
                    List<Long> orderIdList = list.stream().map(OrderTimeTaskDTO::getParkingOrderId).collect(Collectors.toList());

                    try {
                        orderDelayFacade.delayDealOutTimeAndStartOrder(orderIdList, 15L);
                    } catch (Exception e) {
                        try {
                            orderDelayFacade.delayDealOutTimeAndStartOrder(orderIdList, 15L);
                        } catch (Exception exception) {
                            log.error("deal out time order error", exception);
                            DingAlarmUtils.alarmException("delayOutTimeOrderTaskErr" + e.getMessage());
                        }
                    }
                    orderIdList.forEach(OrderDelayJobUtil.needNoticeOrderId::remove);

                }, runMinute, TimeUnit.MINUTES);


            });

        }
        //先提醒后进行补偿
        orderDelayFacade.compensate(BeanConvertorUtils.copyList(parkingOrderPOList, ParkingOrderDTO.class));
    }

    /**
     * 确认上车，确认下车  权限校验
     *
     * @param userId         userId
     * @param parkingOrderId orderId
     */
    @Override
    public void orderPermissionCheck(Long userId, Long parkingOrderId) {
        Long orderPeerCount = orderPeerService.getOrderPeerByOrderIdAndUserId(parkingOrderId, userId);
        if (orderPeerCount == 0) {
            log.error("无权操作订单号为{}的订单", parkingOrderId);
            throw new BusinessException(BizResultCodeEnum.PERMISSION_DENIED.getMsg());
        }
    }

    /**
     * 乘客确定同行
     *
     * @param orderId     订单id
     * @param driverId    司机id
     * @param hitchhikeId 乘客id
     */
    private void hitchhikeRequestMessage(Long orderId, Long driverId, Long hitchhikeId) {
        Map<Long, UserInfoPO> userGroupByUserIdMap = userProfileService.getUserInfoByUserIdList(
                Arrays.asList(driverId, hitchhikeId)).stream()
                .collect(Collectors.toMap(UserInfoPO::getUserId, userInfoPO -> userInfoPO));
        UserInfoPO driverUser = userGroupByUserIdMap.get(driverId);
        UserInfoPO hitchhikeUser = userGroupByUserIdMap.get(hitchhikeId);
        if (ObjectUtils.isEmpty(driverUser)) {
            throw new BusinessException("未找到" + driverId + "对应的用户详细");
        }
        if (ObjectUtils.isEmpty(hitchhikeUser)) {
            throw new BusinessException("未找到" + hitchhikeId + "对应用户详细");
        }
        String message = MessageConstant.getHitchhikeRequestMessage
                (hitchhikeUser.getName(), hitchhikeUser.getJobNumber());
        dingTalkMessageFacade.asyncSendCard
                (new CardMessageDTO().setMessage(message).setOpenIdList
                        (Collections.singletonList(driverUser.getOpenId()))
                        .setUrl(dingTalkProperties.getOrderDetailUrl(orderId)), MessageConstant.PARKING_TITLE);
    }

    /**
     * 司机确认同行
     *
     * @param orderId     订单id
     * @param driverId    司机id
     * @param hitchhikeId 乘客id
     */
    private void passRequestMessage(Long orderId, Long driverId, Long hitchhikeId) {
        Map<Long, UserInfoPO> userGroupByUserIdMap = userProfileService.getUserInfoByUserIdList(
                Arrays.asList(driverId, hitchhikeId)).stream()
                .collect(Collectors.toMap(UserInfoPO::getUserId, userInfoPO -> userInfoPO));
        UserInfoPO driverUser = userGroupByUserIdMap.get(driverId);
        UserInfoPO hitchhikeUser = userGroupByUserIdMap.get(hitchhikeId);
        if (ObjectUtils.isEmpty(driverUser)) {
            throw new BusinessException("未找到" + driverId + "对应的用户详细");
        }
        if (ObjectUtils.isEmpty(hitchhikeUser)) {
            throw new BusinessException("未找到" + hitchhikeId + "对应用户详细");
        }
        String message = MessageConstant.getHitchhikePassRequestMessage
                (driverUser.getName(), driverUser.getJobNumber());
        dingTalkMessageFacade.asyncSendCard
                (new CardMessageDTO().setMessage(message).setOpenIdList
                        (Collections.singletonList(hitchhikeUser.getOpenId()))
                        .setUrl(dingTalkProperties.getOrderDetailUrl(orderId)), MessageConstant.PARKING_TITLE);
    }

    /**
     * 司机取消
     *
     * @param orderId     订单id
     * @param hitchhikeId 乘客id
     * @param orderTime   订单时间
     */
    private void driverCancelMessage(Long orderId, Long hitchhikeId, Long driverId, Date orderTime) {
        Map<Long, UserInfoPO> userGroupByUserIdMap = userProfileService.getUserInfoByUserIdList(
                Arrays.asList(driverId, hitchhikeId)).stream()
                .collect(Collectors.toMap(UserInfoPO::getUserId, userInfoPO -> userInfoPO));
        UserInfoPO driverUser = userGroupByUserIdMap.get(driverId);
        UserInfoPO hitchhikeUser = userGroupByUserIdMap.get(hitchhikeId);
        if (ObjectUtils.isEmpty(driverUser)) {
            throw new BusinessException("未找到" + driverId + "对应的用户详细");
        }
        if (ObjectUtils.isEmpty(hitchhikeUser)) {
            throw new BusinessException("未找到" + hitchhikeId + "对应用户详细");
        }
        if (ObjectUtils.isEmpty(hitchhikeUser)) {
            throw new BusinessException("未找到" + hitchhikeId + "对应用户详细");
        }
        String message = MessageConstant.getDriverCancelMessage
                (driverUser.getName(), driverUser.getJobNumber(), orderTime);
        dingTalkMessageFacade.asyncSendCard
                (new CardMessageDTO().setMessage(message).setOpenIdList
                        (Collections.singletonList(hitchhikeUser.getOpenId()))
                        .setUrl(dingTalkProperties.getOrderDetailUrl(orderId)), MessageConstant.PARKING_TITLE);
    }

    /**
     * 乘客取消
     *
     * @param orderId   订单id
     * @param driverId  乘客id
     * @param orderTime 订单时间
     */
    private void hitchhikeCancelMessage(Long orderId, Long driverId, Long hitchhikeId, Date orderTime) {
        Map<Long, UserInfoPO> userGroupByUserIdMap = userProfileService.getUserInfoByUserIdList(
                        Arrays.asList(driverId, hitchhikeId)).stream()
                .collect(Collectors.toMap(UserInfoPO::getUserId, userInfoPO -> userInfoPO));
        UserInfoPO driverUser = userGroupByUserIdMap.get(driverId);
        UserInfoPO hitchhikeUser = userGroupByUserIdMap.get(hitchhikeId);
        if (ObjectUtils.isEmpty(driverUser)) {
            throw new BusinessException("未找到" + driverId + "对应的用户详细");
        }
        if (ObjectUtils.isEmpty(hitchhikeUser)) {
            throw new BusinessException("未找到" + hitchhikeId + "对应用户详细");
        }
        String message = MessageConstant.getPassengerCancelMessage
                (hitchhikeUser.getName(), hitchhikeUser.getJobNumber(), orderTime);
        dingTalkMessageFacade.asyncSendCard
                (new CardMessageDTO().setMessage(message).setOpenIdList
                        (Collections.singletonList(driverUser.getOpenId()))
                        .setUrl(dingTalkProperties.getOrderDetailUrl(orderId)), MessageConstant.PARKING_TITLE);
    }

    /**
     * 获取某一天指定时间
     *
     * @param time      某一天的时间戳
     * @param clockTime 指定
     * @return
     */
    private Date getSpecifiedTime(Date time, Integer clockTime) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(time);
        calendar.set(Calendar.HOUR_OF_DAY, clockTime);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        return calendar.getTime();
    }

    /**
     * 司机限制发布两单
     *
     * @param collect         订单List
     * @param orderTime       时间
     * @param twelveClockTime 指定的时间,几点钟
     */
    private void checkOrderCommitTimes(List<ParkingOrderPO> collect, Date orderTime, Date twelveClockTime) {
        if (collect.size() >= 2) {
            // 超过两单，不能继续发单
            throw new BusinessException(BizResultCodeEnum.MORE_THAN_TWO_ORDER.getMsg());
        }
        if (twelveClockTime.after(orderTime) && collect.stream().anyMatch(item -> twelveClockTime.after(item.getOrderTime()))) {
            // 上午超过1单
            throw new BusinessException(BizResultCodeEnum.MORE_THAN_TWO_ORDER.getMsg());
        }
        if (twelveClockTime.before(orderTime) && collect.stream().anyMatch(item -> twelveClockTime.before(item.getOrderTime()))) {
            // 下午超过1单
            throw new BusinessException(BizResultCodeEnum.MORE_THAN_TWO_ORDER.getMsg());
        }
    }

    /**
     * 获取带头像的PassengerPeerBO
     *
     * @param userProfilePOS profile列表
     * @param orderPeerPOS   同行列表
     * @return
     */
    private List<PassengerPeerBO> getPassengerBOWithAvatar(List<UserProfilePO> userProfilePOS, List<OrderPeerPO> orderPeerPOS) {
        Map<Long, OrderPeerPO> orderPeerMap = orderPeerPOS.stream().collect(Collectors.toMap(OrderPeerPO::getUserId, orderPeer -> orderPeer));
        return userProfilePOS.stream().map(item -> {
            PassengerPeerBO passengerPeerBO = BeanConvertorUtils.map(item, PassengerPeerBO.class);
            passengerPeerBO.setOrderPeerId(orderPeerMap.get(item.getUserId()).getOrderPeerId()).setTitle(TitleTypeEnum.PASSENGER_TITLE.getMsg());
            return passengerPeerBO;
        }).collect(Collectors.toList());
    }

    /**
     * 确认同行，检测权限，
     *
     * @param userId         userId
     * @param parkingOrderId orderId
     * @param driver         订单
     * @param byId           同行记录
     */
    private void checkPermission(Long userId, Long parkingOrderId, ParkingOrderPO driver, OrderPeerPO byId) {
        // 检查有没有订单
        EmptyUtils.checkOrder(driver, parkingOrderId, "确认同行");
        // 同行记录表 orderPeerId查询
        if (ObjectUtils.isEmpty(byId)) {
            throw new BusinessException(BizResultCodeEnum.NO_PEER_PASSENGER.getMsg());
        }
        // 同一个时间段只能被一个司机确认同行
        // 1. 判断某天上午下午
        // 2. 根据userId + state + 时间段
        Date orderTime = driver.getOrderTime();
        Date twelveClockTime = this.getSpecifiedTime(orderTime, 12);
        Date beginTime;
        Date endTime;
        if (DateUtil.isAM(orderTime)) {
            beginTime = DateUtil.beginOfDay(orderTime);
            endTime = twelveClockTime;
        } else {
            beginTime = twelveClockTime;
            endTime = DateUtil.endOfDay(orderTime);
        }
        if (orderPeerService.getUserConfirmedByOtherDriver(byId.getUserId(), beginTime, endTime) > 0) {
            throw new BusinessException(BizResultCodeEnum.ORDER_PASSENGER_HAS_BEEN_CONFIRMED_BY_OTHER_DRIVER.getMsg());
        }
        // 权限校验
        if (!driver.getUserId().equals(userId)) {
            // 不是司机身份，当前无法操作,提示语
            throw new BusinessException(BizResultCodeEnum.PERMISSION_DENIED.getMsg());
        }
        // 判断 订单表 状态 = 未开始
        if (!driver.getOrderState().equals(OrderStateEnum.ORDER_NOT_START.getCode())) {
            // 状态不是未开始提示语
            throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH.getMsg());
        }
        // 目前逻辑：已确认+已取消请求搭车   新增乘客已确认上车
        if (byId.getRecordState().equals(OrderPeerStateEnum.ORDER_PEER_CONFIRMED.getCode()) || byId.getRecordState().equals(OrderPeerStateEnum.ORDER_PEER_CANCEL_REQUEST.getCode()) || OrderPeerStateEnum.ORDER_PEER_CONFIRM_RIDING.getCode().equals(byId.getRecordState())) {
            // 提示语
            throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH.getMsg());
        }
    }

    /**
     * 司机取消订单权限校验
     *
     * @param order          order实体类
     * @param userId         userId
     * @param parkingOrderId orderId
     */
    private void cancelDriverCheckPermission(ParkingOrderPO order, Long userId, Long parkingOrderId) {
        // 校验订单是否存在
        EmptyUtils.checkOrder(order, parkingOrderId, "取消订单");
        // 不是司机身份
        if (!order.getUserId().equals(userId)) {
            throw new BusinessException(BizResultCodeEnum.PERMISSION_DENIED.getMsg());
        }
        // 判断 订单表 状态 != 未开始/进行中 时,提示 “该状态下不能取消订单”
        if (!order.getOrderState().equals(OrderStateEnum.ORDER_NOT_START.getCode()) && !order.getOrderState().equals(OrderStateEnum.ORDER_ING.getCode())) {
            throw new BusinessException(BizResultCodeEnum.CANNOT_CANCEL_IN_CURRENT_STATE.getMsg());
        }
    }

    /**
     * 获取订单记录权限校验
     *
     * @param userType 用户类型
     */
    private void orderRecordCheckPermission(Integer userType) {
        if (!userType.equals(UserTypeEnum.DRIVER.getCode()) && !userType.equals(UserTypeEnum.PASSENGER.getCode())) {
            // 不是司机不是乘客，恶意请求
            throw new BusinessException(BizResultCodeEnum.PERMISSION_DENIED.getMsg());
        }
    }

    /**
     * 获取我的订单记录：返回评价状态和已完成订单用户id
     *
     * @param orderList 订单列表
     * @return 订单记录
     */
    private List<OrderRecordBO> getOrderRecordCollect(OrderRecordDTO orderRecordDTO, List<ParkingOrderPO> orderList) {
        List<Long> orderIds = orderList.stream().map(ParkingOrderPO::getParkingOrderId).collect(Collectors.toList());
        List<ParkingEvaluatePO> parkingEvaluatePOS = parkingEvaluateService.queryEvaluateByOrderIds(orderIds);

        // 存map  key:parkingOrderId value:List<parkingEvaluate>
        Map<Long, List<ParkingEvaluatePO>> map = parkingEvaluatePOS.stream().collect(Collectors.groupingBy(ParkingEvaluatePO::getParkingOrderId));
        // 为返回数据组装评价状态
        List<OrderRecordBO> collect = orderList.stream().map(item -> {
            OrderRecordBO orderRecordBO = BeanConvertorUtils.map(item, OrderRecordBO.class);
            orderRecordBO.setEvaluateState(EvaluateEnum.NOT_EVALUATED.getCode());
            List<ParkingEvaluatePO> lis = map.get(item.getParkingOrderId());
            // 评价状态判断司机还是乘客 evaluateType: 1评价司机 2：评价乘客
            // 司机类型
            if (CollectionUtils.isNotEmpty(lis) && orderRecordDTO.getUserType().equals(UserTypeEnum.DRIVER.getCode())) {
                lis.forEach(i -> {
                    // 评价乘客
                    if (i.getEvaluateType().equals(UserTypeEnum.DRIVER.getCode())) {
                        orderRecordBO.setEvaluateState(i.getIsEvaluate());
                    }
                });
                // 乘客类型
            } else if (CollectionUtils.isNotEmpty(lis) && orderRecordDTO.getUserType().equals(UserTypeEnum.PASSENGER.getCode())) {
                lis.forEach(i -> {
                    // 评价司机
                    if (i.getEvaluateType().equals(UserTypeEnum.PASSENGER.getCode())) {
                        orderRecordBO.setEvaluateState(i.getIsEvaluate());
                    }
                });
            }
            orderRecordBO.setOrderTimeShow(DateFormatUtils.format(item.getOrderTime(), "M月d日 HH:mm"));
            return orderRecordBO;
        }).collect(Collectors.toList());
        // 已完成订单组装用户id
        List<Long> ordersFinish = collect.stream().filter(o -> Arrays.asList(OrderStateEnum.ORDER_COMPLETE.getCode(), OrderStateEnum.ORDER_OUT_TIME.getCode()).contains(o.getOrderState())).map(OrderRecordBO::getParkingOrderId).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(ordersFinish)) {
            List<OrderPeerPO> orderFinishList = orderPeerService.getOrderPeerByOrderListAndStatusList(ordersFinish, Arrays.asList(OrderPeerStateEnum.ORDER_PEER_COMPLETE.getCode(), OrderPeerStateEnum.ORDER_PEER_OUT_TIME.getCode()));
            Map<Long, OrderPeerPO> orderPeerMap = orderFinishList.stream().collect(Collectors.toMap(OrderPeerPO::getParkingOrderId, o -> o));
            collect = collect.stream().peek(o -> {
                // 为已完成订单组装乘客id
                if (Arrays.asList(OrderStateEnum.ORDER_COMPLETE.getCode(), OrderStateEnum.ORDER_OUT_TIME.getCode()).contains(o.getOrderState())) {
                    Long passengerUserId = orderPeerMap.getOrDefault(o.getParkingOrderId(), new OrderPeerPO()).getUserId();
                    o.setPassengerUserId(passengerUserId);
                }
            }).collect(Collectors.toList());
        }
        return collect;
    }

    /**
     * 获取我的订单记录：根据userType获取乘客和司机的订单列表
     *
     * @param userId   userId
     * @param userType 用户类型
     * @param page     page
     * @return 分页对象
     */
    private IPage<ParkingOrderPO> getPageList(Long userId, Integer userType, Page page) {
        IPage<ParkingOrderPO> pageList;
        if (userType.equals(UserTypeEnum.DRIVER.getCode())) {
            pageList = parkingOrderService.getOrderList(page, userId);
        } else {
            List<OrderPeerPO> orderPeerPOS = orderPeerService.queryOrderPeerListByUserIdAndState(userId, Arrays.asList(OrderPeerStateEnum.ORDER_PEER_FAIL.getCode(), OrderPeerStateEnum.ORDER_PEER_CANCEL_REQUEST.getCode(), OrderPeerStateEnum.ORDER_PEER_REQUESTED.getCode()));
            if (CollectionUtils.isEmpty(orderPeerPOS)) {
                return null;
            }
            List<Long> ids = orderPeerPOS.stream().map(OrderPeerPO::getParkingOrderId).distinct().collect(Collectors.toList());
            pageList = parkingOrderService.getOrderListByOrderIds(page, ids);
        }
        return pageList;
    }

    /**
     * 显示距离逻辑
     *
     * @param dis
     * @return
     */
    private String showDistance(Double dis) {
        return dis < MIN_DISTANCE ? MIN_DISTANCE_SHOW : (dis > MIN_DISTANCE && dis < MAX_DISTANCE) ? MAX_DISTANCE_SHOW : String.format("%.2f", dis) + UNIT_MATHEMATICS;
    }
}
