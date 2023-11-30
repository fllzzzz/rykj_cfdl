package com.cf.parking.services.job.parking;

import cn.hutool.core.date.DateUtil;
import com.alibaba.fastjson.JSON;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.facade.constant.RedisConstant;
import com.cf.parking.services.constant.ParkingConstants;
import com.cf.parking.services.enums.LotteryResultStateEnum;
import com.cf.parking.services.job.annotation.TaskLock;
import com.cf.parking.services.service.LotteryResultService;
import com.cf.parking.services.service.UserSpaceService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import javax.annotation.Resource;
import java.util.Date;
import java.util.List;


/**
 * 车位定时任务
 *
 * @author think
 */
@Slf4j
@Component
public class UserSpaceTask {

    @Resource
    private UserSpaceService userSpaceService;

    @Resource
    private LotteryResultService lotteryResultService;


    /**
     * 删除过期车数据
     */
    @Scheduled(cron = "0 1 0 * * ? ") //间隔1天
    @TaskLock(key = RedisConstant.PARKING_EXPIRED_LOCK_KEY)
    public void dealExpiredSpace() {
        String time = DateUtil.format(new Date(), ParkingConstants.SHORT_DATE_FORMAT);
        log.info("删除过期车定时任务：{}", time);
        userSpaceService.deleteExpiredSpace(time);
    }


    /**
     * 针对一人多库的任务，是在生效期开始当天进行下发闸机
     */
    @Scheduled(cron = "0 0 0/1 * * ? ") //间隔1小时
    @TaskLock(key = RedisConstant.PARKING_DOWN_LOCK_KEY)
    public void parkingDown() {
            String time = DateUtil.format(new Date(), ParkingConstants.SHORT_DATE_FORMAT);
            log.info("按照表里的定时器时间进行下发闸机：{}", time);
            userSpaceService.parkingDownOnStartTtime(time);
    }


    /**
     * 把车位表里的数据同步到闸机系统
     */
    @Scheduled(cron = "0 0 0/1 * * ? ") //间隔1小时
    @TaskLock(key = RedisConstant.PARKING_SYNC_LOCK_KEY)
    public void syncSpace() {
        userSpaceService.syncSpace();
    }


    /**
     * 判断摇号结果表中的数据是否都已下发闸机成功，是的话就更新结果状态
     */
    @Scheduled(cron = "0 0/30 * * * ? ") //间隔30分钟
    @TaskLock(key = RedisConstant.PARKING_RESULT_LOCK_KEY)
    public void syncLotteryResultState() {
        //如果都同步成功，则吧状态改成待发布
        List<LotteryResultPO> resultList = lotteryResultService.selectResultListByState(LotteryResultStateEnum.CONFIRM_IN_PROCESS.getState());
        log.info("确认中的结果数据{}", JSON.toJSONString(resultList));
        resultList.forEach(result -> {
            long num = userSpaceService.queryUnSyncListByBatch(result.getBatchId(), result.getRoundId());
            log.info("batch={},round={}未同步成功的数据量：{}", result.getBatchId(), result.getRoundId(), num);
            if (num == 0) {
                result.setState(LotteryResultStateEnum.UNARCHIVED.getState());
                result.setUpdateTm(new Date());
                lotteryResultService.updateById(result);
                log.info("更新摇号结果表：{}", JSON.toJSONString(result));
            }
        });
    }

}
