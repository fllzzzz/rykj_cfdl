package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.facade.facade.LotteryResultDetailFacade;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 摇号结果详情Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Api(tags = "摇号结果详情模块——摇号系统",description = "此查询模块已在批次和结果模块中执行")
@Slf4j
@RestController
@RequestMapping("/lottery/resultDetail")
public class LotteryResultDetailController
{
    @Resource
    private LotteryResultDetailFacade lotteryResultDetailFacade;

}
