package com.cf.parking.services.job.aspect;

import com.cf.parking.services.job.annotation.TaskLock;
import com.cf.support.utils.DingAlarmUtils;
import com.cf.support.utils.RedisUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.slf4j.MDC;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.lang.reflect.Method;
import java.util.UUID;

/**
 * @Classname CommitAspect
 * @Date 2022/11/4 11:04
 * @Created by csy
 */

@Aspect
@Component
@Slf4j
public class TaskLockAspect {
    @Resource
    private RedisUtil redisUtil;

    @Pointcut("@annotation(com.cf.parking.services.job.annotation.TaskLock)")
    public void taskLockPointCut() {
    }

    @Around("taskLockPointCut()")
    public Object around(JoinPoint joinPoint) throws Throwable {
        String methodName = joinPoint.getSignature().getName();
        Class<?> clazz = joinPoint.getTarget().getClass();
        //获取方法签名(通过此签名获取目标方法信息)
        MethodSignature ms = (MethodSignature) joinPoint.getSignature();
        Method method = clazz.getDeclaredMethod(ms.getName(), ms.getParameterTypes());
        TaskLock annotation = method.getAnnotation(TaskLock.class);
        Scheduled scheduled=method.getAnnotation(Scheduled.class);
        if (ObjectUtils.isEmpty(scheduled)){
            return ((ProceedingJoinPoint) joinPoint).proceed();
        }
        Object result = null;
        if (ObjectUtils.isNotEmpty(annotation)) {
            String lockKey = annotation.key();
            log.info("methodName={}",methodName);

            if(!redisUtil.lock(lockKey, "1", 300)){
                log.info("{}repeat", methodName);
                return null;
            }
            try {

                String traceId = UUID.randomUUID().toString().replaceAll("-", "").toUpperCase();
                MDC.put("traceId", traceId);

                log.info("------------{}start--------------", methodName);

                result = ((ProceedingJoinPoint) joinPoint).proceed();
                log.info("------------{}end--------------", methodName);
            } catch (Exception e) {
                log.error("{}Err", methodName, e);
                DingAlarmUtils.alarmException(methodName + "Err" + e.getMessage());
            } finally {
                MDC.clear();
            }
        }
        return result;
    }
}
