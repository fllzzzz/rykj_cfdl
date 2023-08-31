package com.cf.parking.services.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.po.NotParkingRecordPO;
import com.cf.parking.dao.mapper.NotParkingRecordMapper;
import org.springframework.stereotype.Service;

/**
 * @author csy
 * @date 2023-03-27 09:16:26
 *
 * @description 僵尸车扫描记录表
 */
@Service
public class NotParkingRecordService extends ServiceImpl<NotParkingRecordMapper, NotParkingRecordPO>  implements IService<NotParkingRecordPO> {


}

