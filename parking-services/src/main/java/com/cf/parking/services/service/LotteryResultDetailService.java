package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryResultDetailMapper;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author
 * @date 2023/9/8
 */
@Service
public class LotteryResultDetailService extends ServiceImpl<LotteryResultDetailMapper, LotteryResultDetailPO> implements IService<LotteryResultDetailPO> {

    @Resource
    private LotteryResultDetailMapper mapper;
    
    @Resource
    private LotteryResultService lotteryResultService;

    /**
     * 根据结果id查询对应的结果详情
     * @param page
     * @param resultId
     * @return
     */
    public PageResponse<LotteryResultDetailBO> selectDetailListByResultId(Page<LotteryResultDetailPO> page, Long resultId) {
        LambdaQueryWrapper<LotteryResultDetailPO> detailQueryWrapper = new LambdaQueryWrapper<LotteryResultDetailPO>()
                .eq(LotteryResultDetailPO::getResultId, resultId)
                .orderByAsc(LotteryResultDetailPO::getCreateTm);

        Page<LotteryResultDetailPO> poPage = mapper.selectPage(page, detailQueryWrapper);
        List<LotteryResultDetailBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryResultDetailBO.class);
        return PageUtils.toResponseList(page,boList);
    }

	/**
	 * 根据摇号id查询摇号结果列表
	 * @param resultId
	 * @return
	 */
	public List<LotteryResultDetailPO> queryDetailListByResultId(Long resultId) {
		return mapper.selectList(new LambdaQueryWrapper<LotteryResultDetailPO>()
					.eq(LotteryResultDetailPO::getResultId, resultId)
				);
	}

	/**
	 * 查询该批次下已中奖的人员工号
	 * @param batchId
	 * @return
	 */
	public List<String> querySpaceListByBatchId(Long batchId) {
		List<Long> resultIdList = lotteryResultService.queryResultListByBatchId(batchId);
		
		return CollectionUtils.isEmpty(resultIdList) ? Collections.emptyList() : mapper.selectList(new LambdaQueryWrapper<LotteryResultDetailPO>()
					.in(LotteryResultDetailPO::getResultId, resultIdList)
				).stream().map(item -> item.getUserJobNumber()).collect(Collectors.toList());
	}

	/**
	 * 在结果列表中查询某个用户的结果记录
	 * @param userId
	 * @param resultIds
	 * @return
	 */
	public LotteryResultDetailPO selectUserDetailByResultIds(Long userId, List<Long> resultIds) {
		return mapper.selectOne(new LambdaQueryWrapper<LotteryResultDetailPO>()
				.in(LotteryResultDetailPO::getResultId,resultIds)
				.eq(LotteryResultDetailPO::getUserId,userId)
				.last(" limit 1 ")
				);
	}
}
