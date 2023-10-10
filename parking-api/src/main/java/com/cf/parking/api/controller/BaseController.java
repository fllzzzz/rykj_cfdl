package com.cf.parking.api.controller;

import javax.annotation.Resource;

import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.RestController;

import com.alibaba.fastjson.JSON;
import com.cf.support.authertication.UserAuthenticationServer;
import com.cf.support.authertication.token.dto.UserSessionDTO;
import com.cf.support.exception.BusinessException;

import lombok.extern.slf4j.Slf4j;



@Slf4j
@RestController
public class BaseController {

	 @Resource
	 private UserAuthenticationServer userAuthenticationServer;

	 private UserSessionDTO getUser() {
	     return userAuthenticationServer.getCurrentUser();
		// UserSessionDTO user = new UserSessionDTO();
		// user.setOpenId("CFDL13967");
		// user.setUserId(1668559697477717l);
		// return user;
	 }
	 
	 protected UserSessionDTO getUserSessionDTO() {
	        UserSessionDTO user = getUser();
	        log.info("获取到登陆用户：{}",JSON.toJSONString(user));
	        if (ObjectUtils.isEmpty(user)){
	            throw new BusinessException("请先登录！");
	        }
	        return user;
	 }
}
