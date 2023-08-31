package com.cf.parking.api.controller;

import com.alibaba.fastjson.JSON;
import com.cf.parking.api.annotation.PreventRepeat;
import com.cf.parking.api.request.*;
import com.cf.parking.api.response.*;
import com.cf.parking.facade.bo.*;
import com.cf.parking.facade.dto.*;
import com.cf.parking.facade.enums.OrderDateEnum;
import com.cf.parking.facade.facade.ParkingOrderFacade;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.parking.facade.facade.OrderPeerFacade;
import com.cf.parking.services.utils.EmptyUtils;
import com.cf.parking.services.utils.LongLatitudeUtils;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.authertication.UserAuthentication;
import com.cf.support.authertication.UserAuthenticationServer;
import com.cf.support.authertication.token.dto.UserSessionDTO;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.CFDateUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * @author whx
 * @date 2022/10/19
 */
@Slf4j
@UserAuthentication
@RestController
@RequestMapping("/order")
@Api(tags = "订单模块")
public class ParkingOrderController {

    @Resource
    private UserAuthenticationServer userAuthenticationServer;

    @Resource
    private ParkingOrderFacade parkingOrderFacade;
    @Resource
    private OrderPeerFacade orderPeerFacade;

    private UserSessionDTO getUser() {
        return userAuthenticationServer.getCurrentUser();
    }

    @ApiOperation(value = "获取搭车列表分页接口", notes = "获取搭车列表分页接口")
    @PostMapping("/page")
    public Result<PageResponse<OrderPageRsp>> getOrderPage(@RequestBody OrderPageReq param) {
        RidingOrderDTO orderPageDTO = BeanConvertorUtils.map(param, RidingOrderDTO.class);
        PageResponse<ParkingOrderBO> result = parkingOrderFacade.getOrderPage(getUser().getUserId(), orderPageDTO);
        if (result.getTotal() == 0) {
            return PageUtils.emptyPageResult(result);
        }
        List<OrderPageRsp> orderPageRsps = BeanConvertorUtils.copyList(result.getList(), OrderPageRsp.class);
        return PageUtils.pageResult(result, orderPageRsps);
    }

    @ApiOperation(value = "乘客未完成订单查询接口", notes = "乘客未完成订单查询接口")
    @PostMapping("/pending/passenger")
    public Result<List<PendingRsp>> getPassengerPendingOrder() {
        Long userId = getUser().getUserId();
        List<PendingBO> passengerPendingOrder = orderPeerFacade.getPassengerPendingOrder(userId);
        List<PendingRsp> pendingRsps = BeanConvertorUtils.copyList(passengerPendingOrder, PendingRsp.class);
        return Result.buildSuccessResult(pendingRsps);
    }

    @ApiOperation(value = "乘客请求搭车接口", notes = "乘客请求搭车接口")
    @PreventRepeat
    @PostMapping("/request/confirm")
    public Result requestConfirmOrder(@RequestBody OrderIdReq param) {
        Long parkingOrderId = param.getParkingOrderId();
        EmptyUtils.emptyParkingOrderId(parkingOrderId);
        return parkingOrderFacade.requestConfirmOrder(getUser().getUserId(), parkingOrderId);
    }

    @ApiOperation(value = "乘客取消请求搭车接口", notes = "乘客取消请求搭车接口")
    @PreventRepeat
    @PostMapping("/request/cancel")
    public Result requestCancelOrder(@RequestBody OrderIdReq param) {
        Long parkingOrderId = param.getParkingOrderId();
        EmptyUtils.emptyParkingOrderId(parkingOrderId);
        return parkingOrderFacade.requestCancelOrder(getUser().getUserId(), parkingOrderId);
    }

    @ApiOperation(value = "乘客取消订单接口", notes = "乘客取消订单接口")
    @PreventRepeat
    @PostMapping("/cancel/passenger")
    public Result cancelByPassenger(@RequestBody OrderIdReq param) {
        Long parkingOrderId = param.getParkingOrderId();
        EmptyUtils.emptyParkingOrderId(parkingOrderId);
        return parkingOrderFacade.cancelByPassenger(getUser().getUserId(), parkingOrderId);
    }

    /**
     * 提交订单
     *
     * @param param
     * @return
     */
    @ApiOperation(value = "司机提交订单", notes = "提交订单")
    @PostMapping("/commit")
    @PreventRepeat(state = 1)
    public Result<OrderCommitRsp> orderCommit(@RequestBody OrderCommitReq param) throws Exception {
        log.info("提交订单:[{}]:param{}", CFDateUtils.getCurrentTime(), param.toString());
        Integer orderDate = param.getOrderDate();
        // 校验起始点，目的地、出发时间、乘车人数都为必填，没有填写时需要提示
        if (StringUtils.isBlank(param.getStartProvince()) || StringUtils.isBlank(param.getStartCity()) || StringUtils.isBlank(param.getStartCounty()) || StringUtils.isBlank(param.getStartAddress()) || StringUtils.isBlank(param.getDestProvince()) || StringUtils.isBlank(param.getDestCity()) || StringUtils.isBlank(param.getDestAddress()) || param.getPassengerNum() == 0 || StringUtils.isEmpty(param.getDateTime()) || ObjectUtils.isEmpty(param.getPassengerNum()) || ObjectUtils.isEmpty(orderDate)) {
            return Result.buildResult(BizResultCodeEnum.PARAM_NULL);
        }
        if ((StringUtils.isNotEmpty(param.getRemark()) && param.getRemark().length() > 200) || param.getDestAddress().length() > 100 || param.getStartAddress().length() > 100 || param.getDestProvince().length() > 50 || param.getDestCity().length() > 50 || param.getDestCounty().length() > 50 || param.getStartCity().length() > 50 || param.getStartCounty().length() > 50 || param.getStartProvince().length() > 50) {
            return Result.buildResult(BizResultCodeEnum.PARAM_ERROR);
        }
        // 经纬度校验
        if (ObjectUtils.isEmpty(param.getStartLongitude()) && ObjectUtils.isEmpty(param.getStartLatitude()) && ObjectUtils.isEmpty(param.getDestLongitude()) && ObjectUtils.isEmpty(param.getDestLatitude())) {
            return Result.buildResult(BizResultCodeEnum.PARAM_NULL);
        }
        // 日期校验
        if (orderDate < OrderDateEnum.TODAY.getCode() && orderDate > OrderDateEnum.THE_DAY_AFTER_TOMORROW.getCode()) {
            return Result.buildResult(BizResultCodeEnum.PARAM_ERROR);
        }
        // 时间校验
        Date datetime = CFDateUtils.getDateTime(orderDate, param.getDateTime());
        if (datetime.before(new Date())) {
            return Result.buildResult(BizResultCodeEnum.CANNOT_SETOUT_BEFORE_NOW);
        }
        //半天只能发一单
        ParkingOrderDTO parkingOrderDTO = BeanConvertorUtils.map(param, ParkingOrderDTO.class);
        parkingOrderDTO.setUserId(getUser().getUserId()).setOrderTime(datetime);
        Result<OrderCommitBO> result = parkingOrderFacade.commitOrder(parkingOrderDTO);
        if (result.isSuccess() && ObjectUtils.isNotEmpty(result.getData())) {
            return Result.buildSuccessResult(BeanConvertorUtils.map(result.getData(), OrderCommitRsp.class));
        }
        return BeanConvertorUtils.map(result, Result.class);
    }

    /**
     * 获取订单详情接口
     *
     * @param orderDetailReq
     * @return
     */
    @ApiOperation(value = "获取订单详情", notes = "获取订单详情")
    @PostMapping("/detail")
    public Result<OrderDetailRsp> orderDetail(@RequestBody OrderDetailReq orderDetailReq) {
        Long parkingOrderId = orderDetailReq.getParkingOrderId();
        EmptyUtils.emptyParkingOrderId(parkingOrderId);
        Result<OrderDetailBO> result = parkingOrderFacade.orderDetail(getUser().getUserId(), parkingOrderId);
        if (result.isSuccess() && result.getData() != null) {
            return Result.buildSuccessResult(BeanConvertorUtils.map(result.getData(), OrderDetailRsp.class));
        }
        return BeanConvertorUtils.map(result, Result.class);
    }

    /**
     * 司机获取未完成订单
     *
     * @return
     */
    @ApiOperation(value = "获取司机未完成订单", notes = "获取司机未完成订单")
    @PostMapping("/pending/drive")
    public Result<List<DriverOrderUnfinishedRsp>> pendingDrive() {
        // order_state== 1 || order_state== 2 (状态为 未开始 或 进行中 )，根据用户ID和状态查询离当前日期orderp
        // 最近的两笔订单，按出发时间排序
        // 出发时间过滤
        Long userId = getUser().getUserId();
        List<DriverOrderUnfinishedBO> list = parkingOrderFacade.pendingDrive(userId);
        List<DriverOrderUnfinishedRsp> driverOrderUnfinishedRsps = BeanConvertorUtils.copyList(list, DriverOrderUnfinishedRsp.class);
        return Result.buildSuccessResult(driverOrderUnfinishedRsps);
    }

    /**
     * 司机确认同行
     *
     * @param orderConfirmPeerReq
     * @return
     */
    @ApiOperation(value = "司机确认同行", notes = "司机确认同行")
    @PostMapping("/confirmPeer")
    @PreventRepeat(state = 1)
    public Result orderConfirmPeer(@RequestBody OrderConfirmPeerReq orderConfirmPeerReq) {
        log.info("确认同行:[{}]:param{}", CFDateUtils.getCurrentTime(), orderConfirmPeerReq.toString());
        if (ObjectUtils.isEmpty(orderConfirmPeerReq.getParkingOrderId()) || ObjectUtils.isEmpty(orderConfirmPeerReq.getOrderPeerId())) {
            return Result.buildResult(BizResultCodeEnum.PARAM_NULL);
        }
        Long userId = getUser().getUserId();
        OrderConfirmPeerDTO orderConfirmPeer = new OrderConfirmPeerDTO()
                .setUserId(userId)
                .setParkingOrderId(orderConfirmPeerReq.getParkingOrderId())
                .setOrderPeerId(orderConfirmPeerReq.getOrderPeerId());
        return parkingOrderFacade.orderConfirmPeer(orderConfirmPeer);
    }

    /**
     * 司机取消订单
     *
     * @param param
     * @return
     */
    @ApiOperation(value = "司机取消订单", notes = "司机取消订单")
    @PostMapping("/cancel/drive")
    @PreventRepeat
    public Result cancelDrive(@RequestBody DriverCancelOrderReq param) {
        log.info("取消订单:[{}]:param{}", CFDateUtils.getCurrentTime(), param.toString());
        Long userId = getUser().getUserId();
        return parkingOrderFacade.cancelDriver(userId, param.getParkingOrderId());
    }

    /**
     * 获取我的订单记录
     *
     * @param param
     * @return
     */
    @ApiOperation(value = "获取我的订单记录", notes = "获取我的订单记录")
    @PostMapping("/record")
    public Result<PageResponse<OrderRecordRsp>> orderRecord(@RequestBody OrderRecordReq param) {
        // userType: 2:开车   1：搭车
        if (ObjectUtils.isEmpty(param.getUserType())) {
            return Result.buildResult(BizResultCodeEnum.PARAM_NULL);
        }
        Long userId = getUser().getUserId();

        OrderRecordDTO orderDTO = BeanConvertorUtils.map(param, OrderRecordDTO.class);
        orderDTO.setUserId(userId);

        PageResponse<OrderRecordBO> result = parkingOrderFacade.orderRecord(orderDTO);
        List<OrderRecordRsp> list = BeanConvertorUtils.copyList(result.getList(), OrderRecordRsp.class);
        return PageUtils.pageResult(result, list);
    }

    @PostMapping("/getOnCheck")
    @PreventRepeat
    @ApiOperation(value = "check:乘客确认上车校验", notes = "乘客确认上车校验")
    public Result getOnCheck(@RequestBody CheckReq param) {
        // 订单号校验
        EmptyUtils.emptyParkingOrderId(param.getParkingOrderId());
        // 地理位置合法校验
        LongLatitudeUtils.checkLonLatLegal(Arrays.asList(param.getLongitude(), param.getLatitude()));
        CheckDTO dto = BeanConvertorUtils.map(param, CheckDTO.class);
        return orderPeerFacade.getOnCheck(getUser().getUserId(), dto);
    }

    @PostMapping("/getOffCheck")
    @PreventRepeat
    @ApiOperation(value = "check:乘客确认下车校验", notes = "乘客确认下车校验")
    public Result getOffCheck(@RequestBody CheckReq param) {
        // 订单号校验
        EmptyUtils.emptyParkingOrderId(param.getParkingOrderId());
        // 地理位置合法校验
        LongLatitudeUtils.checkLonLatLegal(Arrays.asList(param.getLongitude(), param.getLatitude()));
        CheckDTO dto = BeanConvertorUtils.map(param, CheckDTO.class);
        return orderPeerFacade.getOffCheck(getUser().getUserId(), dto);
    }

    @PostMapping("/confirmRiding")
    @PreventRepeat
    @ApiOperation(value = "乘客确认上车", notes = "乘客确认上车")
    public Result confirmRiding(@RequestBody ConfirmRidingReq param) {
        // 校验参数
        EmptyUtils.emptyParkingOrderId(param.getParkingOrderId());

        // 地理位置校验
        LongLatitudeUtils.checkLonLatLegal(Arrays.asList(param.getPassengerStartLatitude(), param.getPassengerStartLongitude()));
        ConfirmRidingDTO dto = BeanConvertorUtils.map(param, ConfirmRidingDTO.class);
        dto.setUserId(getUser().getUserId());
        // 操作权限校验
        parkingOrderFacade.orderPermissionCheck(getUser().getUserId(), param.getParkingOrderId());
        return orderPeerFacade.confirmRiding(dto);
    }

    @ApiOperation(value = "乘客确认下车", notes = "乘客确认下车")
    @PreventRepeat
    @PostMapping("/arrive")
    public Result arriveByPassenger(@RequestBody PassengerArriveReq param) {
        EmptyUtils.emptyParkingOrderId(param.getParkingOrderId());
        // 经纬度校验
        LongLatitudeUtils.checkLonLatLegal(Arrays.asList(param.getPassengerDestLongitude(), param.getPassengerDestLatitude()));
        PassengerArriveDTO dto = BeanConvertorUtils.map(param, PassengerArriveDTO.class);
        // 操作权限校验
        parkingOrderFacade.orderPermissionCheck(getUser().getUserId(), param.getParkingOrderId());

        return parkingOrderFacade.arriveByPassenger(getUser().getUserId(), dto);
    }

    @ApiOperation(value = "连续获取当前地理位置信息", notes = "持续定位")
    @PostMapping("/geolocation")
    public Result geolocation(@RequestBody GeolocationReq param) {
        log.info("geolocation,param={}", JSON.toJSONString(param));
        return Result.buildSuccessResult();
    }
}
