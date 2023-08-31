package com.cf.parking.services.facade.impl;

import cn.hutool.core.date.DateUtil;
import com.alibaba.fastjson.JSON;
import com.cf.parking.dao.po.DingNoticeRecordPO;
import com.cf.parking.facade.constant.MessageConstant;
import com.cf.parking.facade.dto.DingNoticeRecordDTO;
import com.cf.parking.facade.facade.DingNoticeRecordFacade;
import com.cf.parking.services.service.DingNoticeRecordService;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.Date;
import java.util.List;

/**
 * @author whx
 * @date 2023/3/31
 */
@Slf4j
@Service
public class DingNoticeRecordFacadeImpl implements DingNoticeRecordFacade {
	@Resource
	private DingNoticeRecordService dingNoticeRecordService;

	@Override
	@Transactional(rollbackFor = Exception.class)
	public void dingNotify(List<DingNoticeRecordDTO> param) {
		//todo 之后删除
		param.forEach(o-> o.setMessage(o.getMessage()+ DateUtil.format(new Date(), MessageConstant.NORM_DATETIME_MINUTE_PATTERN)));
		List<DingNoticeRecordPO> dingNoticeRecordPOS = BeanConvertorUtils.copyList(param, DingNoticeRecordPO.class);
		dingNoticeRecordService.saveDingNoticeRecord(dingNoticeRecordPOS);
		dingNoticeRecordPOS.forEach(dingNoticeRecordPO -> {
			try {
				// 开启子事务
				dingNoticeRecordService.sentDingNotice(dingNoticeRecordPO);
			} catch (Exception e) {
				String content = "send ding notify error: " + JSON.toJSONString(dingNoticeRecordPO);
				log.error(content, e);
//				DingAlarmUtils.alarmException(content);
			}
		});
	}
}
