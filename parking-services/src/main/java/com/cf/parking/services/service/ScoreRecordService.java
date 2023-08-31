package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.ScoreRecordPOMapper;
import com.cf.parking.dao.po.ScoreRecordPO;
import com.cf.parking.facade.bo.AdminScoreRecordBO;
import com.cf.parking.facade.bo.AdminTotalPointsBO;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.support.utils.CFDateUtils;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Date;
import java.util.List;

/**
 * @author whx
 * @date 2022/10/21
 */
@Service
public class ScoreRecordService extends ServiceImpl<ScoreRecordPOMapper, ScoreRecordPO> implements IService<ScoreRecordPO> {
	@Resource
	private ScoreRecordPOMapper scoreRecordPOMapper;

	/**
	 * 查询记录记录分页
	 *
	 * @param page
	 * @param userId
	 * @return
	 */
	public IPage<ScoreRecordPO> getScoresPage(Page page, Long userId) {
		LambdaQueryWrapper<ScoreRecordPO> queryWrapper =
				new LambdaQueryWrapper<ScoreRecordPO>()
						.eq(ScoreRecordPO::getUserId, userId)
						.orderByDesc(ScoreRecordPO::getCreateTm);
		return scoreRecordPOMapper.selectPage(page, queryWrapper);
	}

	/**
	 * 批量插入积分记录
	 *
	 * @param scoreRecordPOS
	 * @return
	 */
	public int insertSelectiveList(List<ScoreRecordPO> scoreRecordPOS) {
		return scoreRecordPOMapper.insertSelectiveList(scoreRecordPOS);
	}

	/**
	 * 查询总积分，已兑换总积分，未兑换总积分
	 *
	 * @return
	 */
	public AdminTotalPointsBO getTotalScores() {
		AdminTotalPointsBO adminTotalPointsBO = new AdminTotalPointsBO();
		//总积分
		LambdaQueryWrapper<ScoreRecordPO> queryWrapper = new QueryWrapper().select("IFNULL(sum(score),0) as totalPoints").lambda();
		Object totalPoints = this.getMap(queryWrapper.ge(ScoreRecordPO::getScore, 0)).get("totalPoints");
		adminTotalPointsBO.setTotalPoints(Integer.parseInt(totalPoints.toString()));
		//已兑换总积分
		LambdaQueryWrapper<ScoreRecordPO> redeemedWrapper = new QueryWrapper().select("IFNULL(sum(score),0) as redeemedPoints").lambda();
		Object redeemedPoints = this.getMap(redeemedWrapper.le(ScoreRecordPO::getScore, 0)).get("redeemedPoints");
		adminTotalPointsBO.setRedeemedPoints(Math.abs(Integer.parseInt(redeemedPoints.toString())));
		//未兑换总积分
		LambdaQueryWrapper<ScoreRecordPO> unchangedWrapper = new QueryWrapper().select("IFNULL(sum(score),0) as unchangedPoints").lambda();
		Object unchangedPoints = this.getMap(unchangedWrapper).get("unchangedPoints");
		adminTotalPointsBO.setUnchangedPoints(Integer.parseInt(unchangedPoints.toString()));
		return adminTotalPointsBO;
	}

	/**
	 * 查询选择时间段内积分排行
	 *
	 * @param beginDate
	 * @param endDate
	 * @return
	 */
	public List<AdminScoreRecordBO> exportTimeScore(Date beginDate, Date endDate) {
		LambdaQueryWrapper<ScoreRecordPO> queryWrapper = new QueryWrapper().select("user_id,sum(score) as totalScore").lambda();
		queryWrapper.between(ScoreRecordPO::getCreateTm, beginDate, CFDateUtils.getDateTimeByDay(endDate, 1))
				.groupBy(ScoreRecordPO::getUserId).last("order by totalScore desc");
		List<ScoreRecordPO> recordPOS = scoreRecordPOMapper.selectList(queryWrapper);
		return BeanConvertorUtils.copyList(recordPOS, AdminScoreRecordBO.class);
	}
}
