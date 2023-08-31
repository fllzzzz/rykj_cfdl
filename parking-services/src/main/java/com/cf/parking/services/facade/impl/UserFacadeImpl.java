package com.cf.parking.services.facade.impl;

import com.alibaba.fastjson.JSON;
import com.cf.parking.dao.po.*;
import com.cf.parking.facade.bo.AuthenticationBO;
import com.cf.parking.facade.bo.UserLoginBO;
import com.cf.parking.facade.bo.UserProfileBO;
import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.facade.constant.UserConstant;
import com.cf.parking.facade.dto.DriverEvaluateDTO;
import com.cf.parking.facade.dto.EvaluateDTO;
import com.cf.parking.facade.dto.UserProfileDTO;
import com.cf.parking.facade.enums.*;
import com.cf.parking.facade.facade.UserFacade;
import com.cf.parking.services.properties.DingTalkProperties;
import com.cf.parking.services.service.*;
import com.cf.parking.services.utils.EmptyUtils;
import com.cf.support.authertication.token.TokenManager;
import com.cf.support.authertication.token.dto.UserSessionDTO;
import com.cf.support.bean.DingTalkBean;
import com.cf.support.bean.IdWorker;
import com.cf.support.entity.SignInfo;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.DingAlarmUtils;
import com.cf.support.utils.RedissonUtil;
import com.dingtalk.api.response.OapiV2UserGetResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.redisson.api.RLock;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.Arrays;
import java.util.Date;
import java.util.concurrent.TimeUnit;

@Service
@Slf4j
public class UserFacadeImpl implements UserFacade {

    @Resource
    private UserService userService;
    @Resource
    private UserProfileService userProfileService;
    @Resource
    private RedissonUtil redissonUtil;
    @Resource
    private TokenManager tokenManager;
    @Resource
    private DingTalkBean dingTalkBean;
    @Resource
    private IdWorker idWorker;
    @Resource
    private OrderPeerService orderPeerService;
    @Resource
    private ParkingEvaluateService parkingEvaluateService;
    @Resource
    private ParkingOrderService parkingOrderService;
    @Resource
    private DingTalkProperties dingTalkProperties;

    private static final String NONCE_STR = "parking";

    @Override
    public Result<UserLoginBO> loginByCode(String code) {
        log.info("进入用户登录接口，code：{}", code);
        if (StringUtils.isBlank(code)) {
            return Result.buildResult(BizResultCodeEnum.PARAM_NULL);
        }
        //根据code获取openid(钉钉userID set user表 openID)
        String openId = dingTalkBean.getUserId(code);
        if (openId == null) {
            return Result.buildResult(BizResultCodeEnum.DING_CODE_USER_ERROR);
        }

        //防止重复注册对openId加锁
        String lockKey = RedisConstant.USER_LOGIN_LOCK_KEY + openId;
        RLock rLock = redissonUtil.getRLock(lockKey);
        try {
            if (!redissonUtil.tryLock(rLock, RedisConstant.USER_LOGIN_LOCK_KEY_WAIT, RedisConstant.USER_LOGIN_LOCK_KEY_EXPIRE, TimeUnit.MINUTES)) {
                log.error("注册用户获取锁失败, 等待重试, openid为{}", openId);
                return Result.buildResult(BizResultCodeEnum.LOCK_ERROR);
            }
            //判断openId是否存在
            UserPO user = userService.selectByOpenId(openId);
            //如果用户不存在则进行注册
            if (null == user) {
                user = new UserPO();
                Long userId = idWorker.nextId();
                user.setUserId(userId);
                user.setOpenId(openId);

                OapiV2UserGetResponse.UserGetResponse userDetail = dingTalkBean.getUserDetail(openId);
                String mobile = userDetail.getMobile();
                String name = userDetail.getName();
                String jobNumber = userDetail.getJobNumber();
                String avatar = userDetail.getAvatar();

                UserProfilePO userProfile = new UserProfilePO();
                userProfile.setUserId(userId);
                userProfile.setMobile(mobile);
                userProfile.setName(name);
                userProfile.setJobNumber(jobNumber);
                userProfile.setAvatar(avatar);
                userProfile.setDriveMark(0);
                userProfile.setRideMark(0);

                userProfileService.saveUserProfile(userProfile);
                userService.saveUser(user);
            } else {
                //判断用户状态为封禁状态不让登录
                if (user.getState().equals(UserConstant.USER_CLOSE)) {
                    log.info("用户:{},已封禁", JSON.toJSONString(user));
                    return Result.buildResult(BizResultCodeEnum.USER_CLOSED);
                }
                //已经存在则更新活跃时间
                UserPO updateUser = new UserPO();
                updateUser.setUserId(user.getUserId());
                updateUser.setLastActiveAt(new Date());
                userService.updateUserInfo(updateUser);
            }
            //token保存
            Long userId = user.getUserId();
            UserSessionDTO sessionDTO = new UserSessionDTO();
            sessionDTO.setUserId(userId);
            sessionDTO.setOpenId(openId);
            sessionDTO.setServerName("parking");

            String token = tokenManager.buildUserToken(sessionDTO);
            if (token == null) {
                log.error("用户登录接口生成token错误,code={},userId={}", code, userId);
                DingAlarmUtils.alarmException("user注册生成token失败,uid=" + userId);
                return Result.buildResult(BizResultCodeEnum.OTHER_SYSTEM_ERROR);
            }
            //用户信息返回
            UserProfilePO profile = userProfileService.getUserProfileByUserId(user.getUserId());
            UserLoginBO resultData = BeanConvertorUtils.map(user, UserLoginBO.class);
            resultData.setToken(token).setJobNumber(profile.getJobNumber()).setMobile(profile.getMobile()).setAvatar(profile.getAvatar()).setName(profile.getName());
            return Result.buildSuccessResult(resultData);
        } catch (Exception e) {
            log.error("用户登录接口异常,入参code：{},e:", code, e);
            return Result.buildResult(BizResultCodeEnum.OTHER_SYSTEM_ERROR);
        } finally {
            try {
                redissonUtil.unlock(rLock);
            } catch (Exception e) {
                log.error("解锁异常,key:[{}],e:", lockKey, e);
            }
        }
    }

    /**
     * 司机评价
     *
     * @param driverEvaluateDTO
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result driverEvaluate(DriverEvaluateDTO driverEvaluateDTO) {
        // 查询同行记录表，看获取出的状态是不是终态
        OrderPeerPO orderPeerPO = orderPeerService.queryPeerRecordByUserIdAndOrderId(driverEvaluateDTO.getUserId(), driverEvaluateDTO.getParkingOrderId());
        // 校验评价权限
        this.checkEvaluatePermission(orderPeerPO, driverEvaluateDTO);
        // 插入评分表
        ParkingEvaluatePO parkingEvaluatePO = new ParkingEvaluatePO();
        BeanConvertorUtils.copy(driverEvaluateDTO, parkingEvaluatePO);
        parkingEvaluatePO.setUserId(driverEvaluateDTO.getDriverUserId()).setEvaluateUserId(driverEvaluateDTO.getUserId()).setIsEvaluate(BooleanEnum.TRUE.getCode()).setEvaluateType(EvaluateTypeEnum.EVALUATE_PASSENGER.getCode()).setEvaluateDesc(driverEvaluateDTO.getEvaluateDesc()).setLevel(parkingEvaluatePO.getLevel());
        if (!parkingEvaluateService.updateEvaluateByOrderIdAndType(parkingEvaluatePO)) {
            throw new BusinessException(BizResultCodeEnum.EVALUATE_ERR);
        }

        // 插入profile表, 搭车评分使用平均值
        Long userId = driverEvaluateDTO.getUserId();
        UserProfilePO userProfile = userProfileService.getById(userId);
        // 判断用户不存在
        EmptyUtils.checkProfile(userProfile, userId, "评价");

        UserProfilePO profile = userProfileService.getUserProfileByUserId(userId);
        // 乘客总搭车评分=乘客总打车评分+当前评分
        Integer rideMark = profile.getRideTotalMark() + driverEvaluateDTO.getLevel();
        // 平均分= 乘客总评分/ (总评次数+1）
        Integer avgMark = rideMark / (profile.getRideEvaluateNum() + 1);
        UserProfilePO userProfilePO = new UserProfilePO().setUserId(userId).setRideMark(avgMark).setRideEvaluateNum(1).setRideTotalMark(driverEvaluateDTO.getLevel());
        userProfileService.updateSelection(userProfilePO);

        return Result.buildSuccessResult();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result passengerEvaluate(EvaluateDTO param) {
        Long parkingOrderId = param.getParkingOrderId();
        Long userId = param.getUserId();
        ParkingOrderPO parkingOrderPO = parkingOrderService.getNotEmptyOrder(userId, parkingOrderId);
        if (!Arrays.asList(OrderStateEnum.ORDER_COMPLETE.getCode(), OrderStateEnum.ORDER_OUT_TIME.getCode()).contains(parkingOrderPO.getOrderState())) {
            throw new BusinessException(BizResultCodeEnum.ORDER_STATE_ERROR.getMsg());
        }
        //重复评价校验
        Long driverId = parkingOrderPO.getUserId();
        this.checkIsEvaluate(driverId, parkingOrderId);

        //用户是否有评价权限
        ParkingEvaluatePO parkingEvaluatePO = parkingEvaluateService.selectOneEvaluate(userId, parkingOrderId);
        //更新评价记录
        ParkingEvaluatePO evaluatePO = BeanConvertorUtils.map(param, ParkingEvaluatePO.class);
        evaluatePO.setParkingEvaluateId(parkingEvaluatePO.getParkingEvaluateId()).setIsEvaluate(EvaluateEnum.NOT_EVALUATED.getCode());
        parkingEvaluateService.updateEvaluateById(evaluatePO, EvaluateEnum.EVALUATED.getCode());
        // 司机 总积分、开车评分更新
        Integer level = param.getLevel();
        UserProfilePO profile = userProfileService.getUserProfileByUserId(driverId);
        // 司机总开车评分=乘客总打车评分+当前评分
        Integer rideMark = profile.getDriveTotalMark() + level;
        // 平均分= 司机总评分/ (总评次数+1）
        Integer avgMark = rideMark / (profile.getDriveEvaluateNum() + 1);
        userProfileService.updateSelection(new UserProfilePO().setUserId(driverId).setDriveMark(avgMark).setDriveEvaluateNum(1).setDriveTotalMark(level));

        return Result.buildSuccessResult();
    }

    /**
     * 获取他人信息
     *
     * @param userId
     * @return
     */
    @Override
    public Result<UserProfileBO> otherProfile(Long userId) {
        UserProfilePO userProfilePO = userProfileService.getById(userId);
        // 检测是否存在该用户
        EmptyUtils.checkProfile(userProfilePO, userId, "获取他人信息");
        String openId = userService.getById(userId).getOpenId();
        // 没有评分默认是五分
        Integer driveMark = userProfilePO.getDriveMark().equals(0) ? 5 : userProfilePO.getDriveMark();
        Integer rideMark = userProfilePO.getRideMark().equals(0) ? 5 : userProfilePO.getRideMark();

        UserProfileBO userProfileBO = BeanConvertorUtils.map(userProfilePO, UserProfileBO.class);
        userProfileBO.setOpenId(openId).setDriveMark(driveMark).setRideMark(rideMark);

        return Result.buildSuccessResult(userProfileBO);
    }

    /**
     * 修改自己信息
     *
     * @param userProfileDTO
     * @return
     */
    @Override
    public Result changeProfile(UserProfileDTO userProfileDTO) {
        UserProfilePO userProfilePO = new UserProfilePO();
        BeanConvertorUtils.copy(userProfileDTO, userProfilePO);
        if (!userProfileService.updateSelfInformation(userProfilePO)) {
            throw new BusinessException(BizResultCodeEnum.STATE_CHANGED_PLEASE_REFRESH);
        }

        return Result.buildSuccessResult();
    }

    @Override
    public UserProfileBO getUserProfile(Long userId) {
        UserProfilePO profile = userProfileService.getById(userId);
        UserPO userPO = userService.getById(userId);
        if (ObjectUtils.isEmpty(userPO) || ObjectUtils.isEmpty(profile)) {
            log.error("个人信息查询异常, userId为{}", userId);
            DingAlarmUtils.alarmException("个人信息查询异常, uid=" + userId);
            throw new BusinessException(BizResultCodeEnum.USER_UN_EXSIT.getMsg());
        }
        //评分为0时，默认返回5
        if (profile.getDriveMark() == 0) {
            profile.setDriveMark(5);
        }
        if (profile.getRideMark() == 0) {
            profile.setRideMark(5);
        }
        UserProfileBO userProfileBO = BeanConvertorUtils.map(profile, UserProfileBO.class);
        userProfileBO.setOpenId(userPO.getOpenId());
        return userProfileBO;
    }

    @Override
    public AuthenticationBO getAuthenticationInfo() {
        SignInfo sign = dingTalkBean.getSign(NONCE_STR, dingTalkProperties.getSignUrl());
        return new AuthenticationBO().setNonceStr(NONCE_STR).setTimeStamp(sign.getTimeStamp()).setSignature(sign.getSign());
    }

    private void checkEvaluatePermission(OrderPeerPO orderPeerPO, DriverEvaluateDTO driverEvaluateDTO) {
        ParkingOrderPO order = parkingOrderService.getOrderByOrderId(driverEvaluateDTO.getParkingOrderId());
        // 不是司机
        if (ObjectUtils.isEmpty(order) || (ObjectUtils.isNotEmpty(order) && (!order.getUserId().equals(driverEvaluateDTO.getDriverUserId())))) {
            throw new BusinessException(BizResultCodeEnum.PERMISSION_DENIED.getMsg());
        }
        if (ObjectUtils.isEmpty(orderPeerPO)) {
            throw new BusinessException(BizResultCodeEnum.ORDER_PEER_ORDER_STATE_MATCH.getMsg());
        }
        // 同行记录是否为空，状态是否已完成
        if (ObjectUtils.isEmpty(orderPeerPO) || (ObjectUtils.isNotEmpty(orderPeerPO) && !Arrays.asList(OrderPeerStateEnum.ORDER_PEER_COMPLETE.getCode(), OrderPeerStateEnum.ORDER_PEER_OUT_TIME.getCode()).contains(orderPeerPO.getRecordState()))) {
            throw new BusinessException(BizResultCodeEnum.CANNOT_EVALUATE.getMsg());
        }
        // 查询同行表看订单号userId是否匹配
        if (orderPeerService.getOrderPeerByOrderIdAndUserId(driverEvaluateDTO.getParkingOrderId(), driverEvaluateDTO.getUserId()).equals(0)) {
            throw new BusinessException(BizResultCodeEnum.CANNOT_EVALUATE.getMsg());
        }

        // 按orderId、司机userId 查询评价表，看是否已评价，已评价直接返回
        this.checkIsEvaluate(driverEvaluateDTO.getUserId(), driverEvaluateDTO.getParkingOrderId());
    }

    /**
     * 检测是否已评价
     *
     * @param userId  被评用户id
     * @param orderId orderId
     */
    private void checkIsEvaluate(Long userId, Long orderId) {
        ParkingEvaluatePO parkingEvaluatePO = parkingEvaluateService.queryEvaluateByOrderIdAndUserId(userId, orderId);
        if (ObjectUtils.isNotEmpty(parkingEvaluatePO) && parkingEvaluatePO.getIsEvaluate().equals(EvaluateEnum.EVALUATED.getCode())) {
            throw new BusinessException(BizResultCodeEnum.ALREADY_EVALUATED.getMsg());
        }
    }

}

