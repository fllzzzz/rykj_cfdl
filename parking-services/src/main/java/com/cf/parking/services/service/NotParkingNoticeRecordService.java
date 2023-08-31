package com.cf.parking.services.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.po.NotParkingNoticeRecordPO;
import com.cf.parking.dao.mapper.NotParkingNoticeRecordMapper;
import org.springframework.stereotype.Service;

/**
 * @author lpy
 * @date 2023-03-29 16:41:26
 *
 * @description 不停车通知记录表
 */
@Service
public class NotParkingNoticeRecordService extends ServiceImpl<NotParkingNoticeRecordMapper, NotParkingNoticeRecordPO>  implements IService<NotParkingNoticeRecordPO> {

}

