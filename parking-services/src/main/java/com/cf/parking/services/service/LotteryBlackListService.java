package com.cf.parking.services.service;

import java.util.List;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.springframework.stereotype.Service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryBlackListMapper;
import com.cf.parking.dao.po.LotteryBlackListPO;



@Service
public class LotteryBlackListService extends ServiceImpl<LotteryBlackListMapper, LotteryBlackListPO> implements IService<LotteryBlackListPO>{

	@Resource
	private LotteryBlackListMapper lotteryBlackListMapper;
	
	
	/**
	 * 查询所有黑名单用户工号
	 * @return
	 */
	public List<String> queryBlackList() {
		return lotteryBlackListMapper.selectList(
				new LambdaQueryWrapper<LotteryBlackListPO>() 
					.eq(LotteryBlackListPO::getType, 1)
				)
				.stream().map(item -> item.getJobNumber()).collect(Collectors.toList());
	}


	/**
	 * 根据用户ID查询黑名单
	 * @param userId
	 * @return
	 */
	public LotteryBlackListPO queryBlackUserInfo(String jobNum) {
		return lotteryBlackListMapper.selectOne(new LambdaQueryWrapper<LotteryBlackListPO>() 
					.eq(LotteryBlackListPO::getJobNumber, jobNum)
					.eq(LotteryBlackListPO::getType, 1)
				);
	}


	/**
	 * 查询所有的领导
	 * @return
	 */
	public List<String> queryAllLeader() {
		return lotteryBlackListMapper.selectList(
				new LambdaQueryWrapper<LotteryBlackListPO>() 
					.eq(LotteryBlackListPO::getType, 2)
			).stream().map(item -> item.getJobNumber()).collect(Collectors.toList());
	}

	
}
