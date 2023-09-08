package com.cf.parking.services.service;


import org.springframework.stereotype.Service;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.LotteryResultDetailMapper;
import com.cf.parking.dao.po.LotteryResultDetailPO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.cf.parking.facade.bo.LotteryResultDetailBO;
import com.cf.support.utils.BeanConvertorUtils;
import javax.annotation.Resource;
import java.util.List;

/**
 * @author
 * @date 2023/9/8
 */
@Service
public class LotteryResultDetailService extends ServiceImpl<LotteryResultDetailMapper, LotteryResultDetailPO> implements IService<LotteryResultDetailPO> {

    @Resource
    private LotteryResultDetailMapper mapper;

    /**
     * 根据结果id查询对应的结果详情
     * @param page
     * @param resultId
     * @return
     */
    public List<LotteryResultDetailBO> selectDetailListByResultId(Page<LotteryResultDetailPO> page, Long resultId) {
        LambdaQueryWrapper<LotteryResultDetailPO> detailQueryWrapper = new LambdaQueryWrapper<LotteryResultDetailPO>()
                .eq(LotteryResultDetailPO::getResultId, resultId)
                .orderByAsc(LotteryResultDetailPO::getCreateTm);

        Page<LotteryResultDetailPO> poPage = mapper.selectPage(page, detailQueryWrapper);
        List<LotteryResultDetailBO> boList = BeanConvertorUtils.copyList(poPage.getRecords(), LotteryResultDetailBO.class);
        return boList;
    }
}
