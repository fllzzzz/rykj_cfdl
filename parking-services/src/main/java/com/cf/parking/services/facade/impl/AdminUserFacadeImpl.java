package com.cf.parking.services.facade.impl;


import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.cf.parking.dao.po.AdminUser;
import com.cf.parking.dao.po.ScoreRecordPO;
import com.cf.parking.dao.po.UserProfilePO;
import com.cf.parking.facade.bo.AdminScoreRecordBO;
import com.cf.parking.facade.bo.LoginBackBO;
import com.cf.parking.facade.dto.AdminScoreOptDTO;
import com.cf.parking.facade.dto.AdminScoresPageDTO;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.parking.facade.facade.AdminUserFacade;
import com.cf.parking.services.service.AdminUserService;
import com.cf.parking.services.service.ScoreRecordService;
import com.cf.parking.services.service.UserProfileService;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.authertication.token.TokenManager;
import com.cf.support.authertication.token.TokenRedisConstant;
import com.cf.support.authertication.token.dto.AdminSessionDTO;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.RedisUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.DigestUtils;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * @author weihui
 * @date 2020/10/17
 */
@Service
@Slf4j
public class AdminUserFacadeImpl implements AdminUserFacade {

    @Resource
    private TokenManager tokenManager;

    @Resource
    private AdminUserService adminUserService;

    @Resource
    private UserProfileService userProfileService;

    @Resource
    private ScoreRecordService scoreRecordService;

    @Resource
    private RedisUtil redisUtil;

    private String serverName = "parking";



    public static void main(String[] args) {
        System.out.println(DigestUtils.md5DigestAsHex("CFDL15244123456".getBytes()));
        System.out.println(DigestUtils.md5DigestAsHex("CFDL13914123456".getBytes()));
        System.out.println(DigestUtils.md5DigestAsHex("CFDL14966123456".getBytes()));
        System.out.println(DigestUtils.md5DigestAsHex("CFDL14965123456".getBytes()));
    }

    @Override
    public Result<LoginBackBO> login(String emplNo, String password) {
        log.info("进入用户登录接口:emplNo:{}", emplNo);
        AdminUser user = adminUserService.getUserByEmplNo(emplNo);
        if (user == null) {
            return Result.buildErrorResult("用户不存在");
        }
        password = DigestUtils.md5DigestAsHex((emplNo + password).getBytes());
        if (!password.equals(user.getPassword())) {
            return Result.buildErrorResult("密码不正确");
        }
        AdminSessionDTO sessionDTO = new AdminSessionDTO();
        sessionDTO.setAdminId(user.getAdminUserId().toString());
        sessionDTO.setAdminName(user.getAdminName());
        sessionDTO.setIsSuper(user.getIsSuper());
        sessionDTO.setEmplNo(emplNo);
        sessionDTO.setServerName(serverName);
        String token = tokenManager.buildAdminToken(sessionDTO);
        LoginBackBO result = new LoginBackBO();
        result.setToken(token);
        result.setAdminName(user.getAdminName());
        result.setIsSuper(user.getIsSuper());
        log.info("用户登录接口返回结果为:{}", JSONObject.toJSONString(result));
        return Result.buildSuccessResult(result);
    }

    @Override
    public Result logout(Long adminId) {
        clearToken(adminId);
        return Result.buildSuccessResult();
    }

    private void clearToken(Long adminId){
        String userKey = TokenRedisConstant.ADMIN_SESSION_PREFIX + serverName + ":" + adminId;
        redisUtil.del(userKey);
    }

    @Override
    public Result updatePwd(String emplNo, String oldPassword, String newPassword) {
        AdminUser user = adminUserService.getUserByEmplNo(emplNo);
        oldPassword = DigestUtils.md5DigestAsHex((emplNo + oldPassword).getBytes());
        if (!oldPassword.equals(user.getPassword())) {
            return Result.buildErrorResult("原密码不正确");
        }
        String password = DigestUtils.md5DigestAsHex((emplNo + newPassword).getBytes());
        adminUserService.updatePwd(password, user.getAdminUserId());
        clearToken(user.getAdminUserId());
        return Result.buildSuccessResult();
    }

    @Override
    public PageResponse<AdminScoreRecordBO> getScoresPage(AdminScoresPageDTO param) {
        IPage scoresPage = userProfileService.getScoresPage(param);
        if (scoresPage.getTotal() == 0) {
            return PageUtils.emptyResponseList(scoresPage);
        }
        List<AdminScoreRecordBO> adminScoreRecordBOS = BeanConvertorUtils.copyList(scoresPage.getRecords(), AdminScoreRecordBO.class);
        return PageUtils.toResponseList(scoresPage, adminScoreRecordBOS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result scoreOpt(AdminScoreOptDTO param) {
        List<Long> userIdList = param.getUserIdList();
        Integer score = param.getScore();
        // 减分校验，所扣分不能大于总分
        userProfileService.checkTotalScoreList(score, userIdList);
        //批量生成积分记录
        List<ScoreRecordPO> recordPOS = BeanConvertorUtils.copyList(userProfileService.selectList(userIdList), ScoreRecordPO.class);
        for (ScoreRecordPO scoreRecordPO : recordPOS) {
            BeanConvertorUtils.copy(param, scoreRecordPO);
        }
        scoreRecordService.insertSelectiveList(recordPOS);
        //批量更新用户总分
        List<UserProfilePO> userProfilePOS = new ArrayList<>();
        userIdList.forEach(userId -> {
            userProfilePOS.add(new UserProfilePO().setTotalScore(score).setUserId(userId));
        });
        if (userProfileService.updateSelectionList(userProfilePOS) == 0) {
            throw new BusinessException(BizResultCodeEnum.SCORE_OPT_FAIL.getMsg());
        }
        return Result.buildSuccessResult();
    }

}
