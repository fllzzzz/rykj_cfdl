package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.po.WhiteListPO;
import com.cf.parking.dao.mapper.WhiteListMapper;
import com.cf.parking.facade.dto.WhiteListBatchDelDTO;
import com.cf.parking.facade.enums.IsDeleteEnum;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author whx
 * @date 2023-03-30 11:02:41
 * @description 白名单记录表
 */
@Service
public class WhiteListService extends ServiceImpl<WhiteListMapper, WhiteListPO> implements IService<WhiteListPO> {

    @Resource
    private WhiteListMapper whiteListMapper;

    /**
     * 白名单批量软删除
     *
     * @param param
     * @return
     */
    public Boolean whiteListBatchDel(WhiteListBatchDelDTO param) {
        LambdaUpdateWrapper<WhiteListPO> updateWrapper = new LambdaUpdateWrapper<WhiteListPO>().in(WhiteListPO::getWhiteListId, param.getWhiteListIds())
                .set(WhiteListPO::getIsDelete, IsDeleteEnum.TRUE.getCode());
        return this.update(updateWrapper);
    }

    /**
     * 模糊查询获取分页
     *
     * @param page
     * @param whiteListPO
     * @return
     */
    public IPage queryPage(Page page, WhiteListPO whiteListPO) {
        return this.page(page, new LambdaQueryWrapper<WhiteListPO>().eq(WhiteListPO::getIsDelete, IsDeleteEnum.FALSE.getCode())
                .like(WhiteListPO::getPlateNo, whiteListPO.getPlateNo()).orderByDesc(WhiteListPO::getCreateTm, WhiteListPO::getWhiteListId));
    }

    /**
     * 获取未删除的白名单车
     *
     * @return List
     */
    public List<String> getWhiteList() {
        return this.list(new LambdaQueryWrapper<WhiteListPO>()
                .eq(WhiteListPO::getIsDelete, IsDeleteEnum.FALSE.getCode())
        ).stream().map(WhiteListPO::getPlateNo).collect(Collectors.toList());
    }

    /**
     * 更新/新增数据
     *
     * @param whiteListPOS
     */
    public void saveWhiteList(List<WhiteListPO> whiteListPOS) {
        whiteListMapper.replaceWhiteList(whiteListPOS);
    }
}

