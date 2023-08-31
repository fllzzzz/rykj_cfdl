package com.cf.parking.services.facade.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.cf.parking.dao.po.UserSpacePO;
import com.cf.parking.dao.po.WhiteListPO;
import com.cf.parking.facade.bo.WhiteListPageBO;
import com.cf.parking.facade.dto.WhiteListBatchDelDTO;
import com.cf.parking.facade.dto.WhiteListAddDTO;
import com.cf.parking.facade.dto.WhiteListPageDTO;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.parking.facade.facade.UserSpaceFacade;
import com.cf.parking.facade.facade.WhiteListFacade;
import com.cf.parking.services.service.UserSpaceService;
import com.cf.parking.services.service.WhiteListService;
import com.cf.parking.services.utils.PageUtils;
import com.cf.support.exception.BusinessException;
import com.cf.support.result.PageResponse;
import com.cf.support.utils.BeanConvertorUtils;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * @author whx
 * @date 2023/3/28
 */
@Service
public class WhiteListFacadeImpl implements WhiteListFacade {
    @Resource
    private WhiteListService whiteListService;

    @Resource
    private UserSpaceService userSpaceService;

    @Override
    public void whiteListBatchDel(WhiteListBatchDelDTO param) {
        if (!whiteListService.whiteListBatchDel(param)) {
            throw new BusinessException(BizResultCodeEnum.LOCK_ERROR.getMsg());
        }
    }

    @Override
    public PageResponse<WhiteListPageBO> page(WhiteListPageDTO param) {
        IPage whiteListPage = whiteListService.queryPage(PageUtils.toPage(param), BeanConvertorUtils.map(param, WhiteListPO.class));
        if (whiteListPage.getTotal() == 0) {
            return PageUtils.emptyResponseList(whiteListPage);
        }
        List<WhiteListPageBO> whiteListPageBOS = BeanConvertorUtils.copyList(whiteListPage.getRecords(), WhiteListPageBO.class);
        return PageUtils.toResponseList(whiteListPage, whiteListPageBOS);
    }

    @Override
    public void whiteListSave(WhiteListAddDTO param) {
        String[] plateNos = param.getPlateNoText().replaceAll("\\s*", "").replace("\r", "").split(",");
        List<UserSpacePO> userSpacePOList = userSpaceService.list(new LambdaQueryWrapper<UserSpacePO>().in(UserSpacePO::getPlateNo, plateNos));
        List<WhiteListPO> whiteListPOS = userSpacePOList.stream().distinct().map(o -> {
            //格式校验
            //this.plateMatcher(plateNo);
            return new WhiteListPO().setPlateNo(o.getPlateNo());
        }).collect(Collectors.toList());
        whiteListService.saveWhiteList(whiteListPOS);
    }

    /**
     * 车牌正则匹配
     *
     * @param plateNo
     */
    private void plateMatcher(String plateNo) {
        String plateRule = "^(([京津沪渝冀豫云辽黑湘皖鲁新苏浙赣鄂桂甘晋蒙陕吉闽贵粤青藏川宁琼使领][A-Z](([0-9]{5}[DF])|([DF]([A-HJ-NP-Z0-9])[0-9]{4})))|([京津沪渝冀豫云辽黑湘皖鲁新苏浙赣鄂桂甘晋蒙陕吉闽贵粤青藏川宁琼使领][A-Z][A-HJ-NP-Z0-9]{4}[A-HJ-NP-Z0-9挂学警港澳使领]))$";
        Pattern pattern = Pattern.compile(plateRule);
        Matcher matcher = pattern.matcher(plateNo.trim());
        if (!matcher.matches()) {
            throw new BusinessException(plateNo + "不符合车牌号格式规范");
        }
    }
}
