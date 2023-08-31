package com.cf.parking.api.aspect;

import com.cf.parking.api.annotation.PreventRepeat;
import com.cf.support.authertication.AdminAuthenticationServer;
import com.cf.support.authertication.UserAuthenticationServer;
import com.cf.support.result.Result;
import com.cf.support.result.ResultCodeEnum;
import com.cf.support.utils.RedisUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.lang.reflect.Method;

/**
 * @Classname CommitAspect
 * @Date 2022/11/4 11:04
 * @Created by csy
 */

@Aspect
@Component
@Slf4j
public class CommitAspect {
    @Resource
    private UserAuthenticationServer userAuthenticationServer;
    @Resource
    private AdminAuthenticationServer adminAuthenticationServer;
    @Resource
    private RedisUtil redisUtil;

    @Pointcut("@annotation(com.cf.parking.api.annotation.PreventRepeat)")
    public void commitPointCut() {
    }

    @Around("commitPointCut()")
    public Object around(JoinPoint joinPoint) throws Throwable {
        String methodName = joinPoint.getSignature().getName();
        Class<?> clazz = joinPoint.getTarget().getClass();
        //获取方法签名(通过此签名获取目标方法信息)
        MethodSignature ms=(MethodSignature)joinPoint.getSignature();
        Method method = clazz.getDeclaredMethod(ms.getName(),ms.getParameterTypes());
        PreventRepeat annotation = method.getAnnotation(PreventRepeat.class);
        if (ObjectUtils.isNotEmpty(annotation)) {
            Long userId = userAuthenticationServer.getCurrentUser().getUserId();
            if (annotation.state() == 2) {
                userId = Long.parseLong(adminAuthenticationServer.getCurrentUser().getAdminId());
            }
            if (ObjectUtils.isEmpty(userId)) {
                return Result.buildErrorResult(ResultCodeEnum.NOT_LOGIN.getMsg());
            }
            String lockKey = StringUtils.isNotBlank(annotation.key()) ? annotation.key() : userId + ":" + methodName;
            if (!redisUtil.lock(lockKey, lockKey, annotation.second())) {
                return Result.buildErrorResult(ResultCodeEnum.REPEAT_SUBMIT_EXPIRATION.getMsg());
            }
        }
        return ((ProceedingJoinPoint) joinPoint).proceed();
    }
}
