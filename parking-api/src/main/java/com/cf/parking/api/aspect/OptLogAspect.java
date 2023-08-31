package com.cf.parking.api.aspect;


import com.alibaba.fastjson.JSON;
import com.cf.parking.api.annotation.AdminOptLogTitle;
import com.cf.parking.dao.po.AdminOptLog;
import com.cf.parking.services.service.AdminOptLogService;
import com.cf.support.utils.IpUtil;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import java.lang.reflect.Method;

/**
 * @author weihui
 * @date 2020/10/20
 */


@Aspect
@Component
@Slf4j
public class OptLogAspect {

    @Resource
    private AdminOptLogService adminOptLogService;

    @Pointcut("@annotation(com.cf.parking.api.annotation.AdminOptLogTitle)")
    public void optLogPointCut() {
    }

    @Around("optLogPointCut()")
    public Object around(JoinPoint joinPoint) throws Throwable {
        long startTime = System.currentTimeMillis();

        //返回值
        Object result = ((ProceedingJoinPoint) joinPoint).proceed();

        HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getRequest();
        String params = "";//请求参数
        if (joinPoint.getArgs() != null && joinPoint.getArgs().length > 0) {
            for (int i = 0; i < joinPoint.getArgs().length; i++) {
                if(joinPoint.getArgs()[i] instanceof MultipartFile){
                    MultipartFile file = (MultipartFile)joinPoint.getArgs()[i];
                    params += JSON.toJSONString(file.getOriginalFilename());
                }else{
                    params += JSON.toJSONString(joinPoint.getArgs()[i]);
                }
            }
        }

        // 从切面织入点处通过反射机制获取织入点处的方法
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        // 获取切入点所在的方法
        Method method = signature.getMethod();
        // 获取操作名称
        AdminOptLogTitle opLog = method.getAnnotation(AdminOptLogTitle.class);
        String optTitle = opLog.value();
        long endTime = System.currentTimeMillis();
        //执行耗时
        int time = (int) (endTime - startTime);

        try {
            //保存操作记录表
            AdminOptLog item = new AdminOptLog();
            item.setExecuteTime(time);
            item.setOptTitle(optTitle);
            item.setIp(IpUtil.getIp(request));
            item.setParams(params);
            item.setResponse(JSON.toJSONString(result));
            String token = request.getHeader("token");
            adminOptLogService.save(item, token);
        } catch (Exception e) {
            log.error("操作日志记录异常,e:",e);
        }
        return result;
    }

}
