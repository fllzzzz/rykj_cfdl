package com.cf.parking.services.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.po.NoticeRecordPO;
import com.cf.parking.dao.mapper.NoticeRecordMapper;
import org.springframework.stereotype.Service;

/**
 * @author lpy
 * @date 2023-03-29 14:31:06
 *
 * @description 通知记录表
 */
@Service
public class NoticeRecordService extends ServiceImpl<NoticeRecordMapper, NoticeRecordPO>  implements IService<NoticeRecordPO> {

}

