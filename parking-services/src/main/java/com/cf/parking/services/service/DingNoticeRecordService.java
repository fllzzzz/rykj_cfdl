package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.DingNoticeRecordMapper;
import com.cf.parking.dao.po.DingNoticeRecordPO;
import com.cf.parking.facade.dto.TextMessageDTO;
import com.cf.parking.facade.enums.NoticedEnum;
import com.cf.parking.facade.facade.DingTalkMessageFacade;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author whx
 * @date 2023-03-31 10:13:21
 * @description 钉钉通知记录表
 */
@Service
public class DingNoticeRecordService extends ServiceImpl<DingNoticeRecordMapper, DingNoticeRecordPO> implements IService<DingNoticeRecordPO> {

	@Resource
	private DingNoticeRecordMapper dingNoticeRecordMapper;
	@Resource
	private DingTalkMessageFacade dingTalkMessageFacade;

	/**
	 * 批量保存，返回主键ID
	 *
	 * @param dingNoticeRecordPOS
	 * @return
	 */
	public List<Long> saveDingNoticeRecord(List<DingNoticeRecordPO> dingNoticeRecordPOS) {
		dingNoticeRecordMapper.insertBatchNotice(dingNoticeRecordPOS);
		return dingNoticeRecordPOS.stream().map(DingNoticeRecordPO::getDingNoticeRecordId).collect(Collectors.toList());
	}

	/**
	 * 更新状态
	 *
	 * @param dingNoticeRecordPO
	 * @return
	 */
	public boolean updateDingStatue(DingNoticeRecordPO dingNoticeRecordPO) {
		return this.update(new LambdaUpdateWrapper<DingNoticeRecordPO>().eq(DingNoticeRecordPO::getDingNoticeRecordId, dingNoticeRecordPO.getDingNoticeRecordId())
				.set(DingNoticeRecordPO::getStatus, NoticedEnum.NOTICED.getCode()));
	}

	/**
	 * 更新状态，发生钉钉通知
	 *
	 * @param dingNoticeRecordPO
	 */
	@Transactional(propagation = Propagation.NESTED, rollbackFor = Exception.class)
	public void sentDingNotice(DingNoticeRecordPO dingNoticeRecordPO) {
		// 更新状态
		this.updateDingStatue(dingNoticeRecordPO);
		// 钉钉通知
		/*List<String> jobNumber = Arrays.asList(dingNoticeRecordPO.getJobNumber());*/
		// TODO 暂时统一通知test
		List<String> jobNumber = Arrays.asList("");
		String message = dingNoticeRecordPO.getMessage();
		TextMessageDTO messageDTO = new TextMessageDTO().setMessage(message).setOpenIdList(jobNumber);
		// 工作通知
//		dingTalkMessageFacade.sendBatchText(Arrays.asList(messageDTO));
		// 群消息
//		DingFacadeClient.reservePush(message);
	}
}