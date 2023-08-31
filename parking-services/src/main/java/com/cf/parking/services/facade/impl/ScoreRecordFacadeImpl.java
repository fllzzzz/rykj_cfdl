package com.cf.parking.services.facade.impl;

import cn.hutool.core.collection.CollectionUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.cf.parking.dao.po.AdminUser;
import com.cf.parking.dao.po.ScoreRecordPO;
import com.cf.parking.dao.po.UserProfilePO;
import com.cf.parking.facade.bo.AdminScoreRecordBO;
import com.cf.parking.facade.bo.ScoreRecordBO;
import com.cf.parking.facade.facade.ScoreRecordFacade;
import com.cf.parking.services.service.AdminUserService;
import com.cf.parking.services.service.ScoreRecordService;
import com.cf.parking.services.service.UserProfileService;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageRequest;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author whx
 * @date 2022/10/21
 */
@Service
@Slf4j
public class ScoreRecordFacadeImpl implements ScoreRecordFacade {

	@Resource
	private ScoreRecordService scoreRecordService;

	@Resource
	private UserProfileService userProfileService;

	@Resource
	private AdminUserService adminUserService;

	@Override
	public PageResponse<ScoreRecordBO> getScoresPage(Long userId, PageRequest param) {
		IPage scoresPage = scoreRecordService.getScoresPage(PageUtils.toPage(param), userId);
		if (scoresPage.getTotal() == 0) {
			return PageUtils.emptyResponseList(scoresPage);
		}
		List<ScoreRecordPO> recordPOS = scoresPage.getRecords();
		List<Long> adminUserIdList = recordPOS.stream().filter(item -> item.getAdminUserId() != 0).map(ScoreRecordPO::getAdminUserId).distinct().collect(Collectors.toList());
		Map<Long, AdminUser> adminUserMap = new HashMap<>();
		if (CollectionUtil.isNotEmpty(adminUserIdList)) {
			adminUserMap = adminUserService.getAdminUserList(adminUserIdList).stream().collect(Collectors.toMap(AdminUser::getAdminUserId, adminUser -> adminUser));
		}
		List<ScoreRecordBO> recordBOS = new ArrayList<>();
		for (ScoreRecordPO recordPO : recordPOS) {
			ScoreRecordBO recordBO = BeanConvertorUtils.map(recordPO, ScoreRecordBO.class);
			recordBO.setCreateTm(DateFormatUtils.format(recordPO.getCreateTm(), "yyyy-MM-dd HH:mm:ss"));
			if (recordPO.getAdminUserId() > 0) {
				AdminUser adminUser = adminUserMap.get(recordPO.getAdminUserId());
				recordBO.setAdminName(ObjectUtils.isEmpty(adminUser.getAdminName()) ? "" : adminUser.getAdminName())
						.setEmplNo(ObjectUtils.isEmpty(adminUser.getEmplNo()) ? "" : adminUser.getEmplNo());
			}
			recordBOS.add(recordBO);
		}
		return PageUtils.toResponseList(scoresPage, recordBOS);
	}

	@Override
	public List<AdminScoreRecordBO> exportTimeScore(Date beginDate, Date endDate) {
		List<AdminScoreRecordBO> adminScoreRecordBOS = scoreRecordService.exportTimeScore(beginDate, endDate);
		if (CollectionUtil.isEmpty(adminScoreRecordBOS)) {
			return new ArrayList<>();
		}
		List<Long> userIdList = adminScoreRecordBOS.stream().map(AdminScoreRecordBO::getUserId).collect(Collectors.toList());
		Map<Long, UserProfilePO> userProfilePOS = userProfileService.selectList(userIdList).stream()
				.collect(Collectors.toMap(UserProfilePO::getUserId, userProfile -> userProfile));
		return adminScoreRecordBOS.stream().map(item -> {
			UserProfilePO userProfile = userProfilePOS.get(item.getUserId());
			item.setName(userProfile.getName()).setJobNumber(userProfile.getJobNumber());
			return item;
		}).collect(Collectors.toList());
	}

}
