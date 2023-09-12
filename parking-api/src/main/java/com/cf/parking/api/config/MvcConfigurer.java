package com.cf.parking.api.config;


import com.cf.support.authertication.interceptor.AdminAuthenticationInterceptor;
import com.cf.support.authertication.interceptor.UserAuthenticationInterceptor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import java.util.ArrayList;
import java.util.List;

@Configuration
public class MvcConfigurer implements WebMvcConfigurer {

    /**
     * 不需要拦截的请求
     */
    private static List<String> execludePathList = new ArrayList<>();

    static {
        execludePathList.add("/error");
        execludePathList.add("/ops/heart");
        execludePathList.add("/ops/ready");
    }

    /**
     * 在添加拦截器之前，先创建该bean，纳入到spring中。
     *      解决拦截器中无法依赖注入的问题
     * @return
     */
    @Bean
    public UserAuthenticationInterceptor getUserAuthenticationInterceptor(){
        return new UserAuthenticationInterceptor();
    }

    /**
     * 在添加拦截器之前，先创建该bean，纳入到spring中。
     *      解决拦截器中无法依赖注入的问题
     * @return
     */
    @Bean
    public AdminAuthenticationInterceptor getAdminAuthenticationInterceptor(){
        return new AdminAuthenticationInterceptor();
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        // 用户拦截器
        registry.addInterceptor(getUserAuthenticationInterceptor()).addPathPatterns("/**").excludePathPatterns(execludePathList);
        // 管理后台拦截器
        registry.addInterceptor(getAdminAuthenticationInterceptor()).addPathPatterns("/**").excludePathPatterns(execludePathList);
    }
}
