package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryResultMapper;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.cf.parking.dao.po.LotteryResultPO;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.support.result.PageResponse;
import org.springframework.stereotype.Service;
import javax.annotation.Resource;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author
 * @date 2023/9/8
 */
@Service
public class LotteryResultService extends ServiceImpl<LotteryResultMapper, LotteryResultPO> implements IService<LotteryResultPO> {

    @Resource
    private LotteryResultMapper mapper;

    @Resource
    private LotteryResultDetailService lotteryResultDetailService;

	@Resource
	private ParkingLotService parkingLotService;


    /**
     * 已结束的摇号批次进行结果查看
     * @param page
     * @param batchId
     * @param roundId
     * @return
     */
    public PageResponse<LotteryResultDetailBO> viewResult(Page<LotteryResultDetailPO> page, Long batchId, Long roundId) {
        //1.根据摇号批次的id和轮数查询结果id
        LotteryResultPO po = mapper.selectOne(new LambdaQueryWrapper<LotteryResultPO>()
                .eq(LotteryResultPO::getBatchId, batchId)
                .eq(LotteryResultPO::getRoundId, roundId));

        if (null == po){
            return null;
        }

        //2.根据结果id查询对应的结果详情
        PageResponse<LotteryResultDetailBO> result =  lotteryResultDetailService.selectDetailListByResultId(page,po.getId());
		result.getList().forEach(bo -> {
			bo.setParkingLotName(parkingLotService.selectParkingLotByCode(bo.getParkingLotCode()).getRegion());
		});
		return result;
    }


	/**
	 * 根据批次查询结果id
	 * @param batchId
	 * @return
	 */
	public List<Long> queryResultListByBatchId(Long batchId) {
		return mapper.selectList(new LambdaQueryWrapper<LotteryResultPO>()
				.eq(LotteryResultPO::getBatchId, batchId)
				).stream().map(item -> item.getId()).collect(Collectors.toList());
	}


	/**
	 * 根据状态查询列表
	 * @param state 状态
	 */
	public List<LotteryResultPO> selectResultListByState(String state) {
		return mapper.selectList(new LambdaQueryWrapper<LotteryResultPO>()
				.eq(LotteryResultPO::getState, state)
				);
	}

	public List<LotteryResultPO> selectResultListByBatchId(Long batchId) {
		return mapper.selectList(new LambdaQueryWrapper<LotteryResultPO>()
				.eq(LotteryResultPO::getBatchId,batchId));
	}
}
