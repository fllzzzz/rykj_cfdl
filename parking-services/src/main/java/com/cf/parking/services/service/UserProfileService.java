package com.cf.parking.services.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.cf.parking.dao.mapper.UserProfilePOMapper;
import com.cf.parking.dao.po.UserInfoPO;
import com.cf.parking.dao.po.UserPO;
import com.cf.parking.dao.po.UserProfilePO;
import com.cf.parking.facade.enums.BizResultCodeEnum;
import com.cf.support.exception.BusinessException;
import com.cf.support.utils.BeanConvertorUtils;
import com.cf.parking.facade.bo.AdminScoreRecordBO;
import com.cf.parking.facade.dto.AdminScoresPageDTO;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author whx
 * @date 2022/10/12
 */


@Service
public class UserProfileService extends ServiceImpl<UserProfilePOMapper, UserProfilePO>
        implements IService<UserProfilePO> {

    @Resource
    private UserProfilePOMapper userProfilePOMapper;
    @Resource
    private UserService userService;

    /**
     * 用户详情信息保存
     */
    public void saveUserProfile(UserProfilePO userProfilePO) {
        userProfilePOMapper.insertSelective(userProfilePO);
    }

    /**
     * 根据id查询用户详情
     */
    public UserProfilePO getUserProfileByUserId(Long userId) {
        return this.getById(userId);
    }

    /**
     * 积分分页查询
     *
     * @param param
     * @return
     */
    public IPage getScoresPage(AdminScoresPageDTO param) {
        LambdaQueryWrapper<UserProfilePO> queryWrapper =
                new LambdaQueryWrapper<UserProfilePO>()
                        .like(StringUtils.isNotEmpty(param.getName()), UserProfilePO::getName, param.getName())
                        .like(StringUtils.isNotEmpty(param.getJobNumber()), UserProfilePO::getJobNumber, param.getJobNumber())
                        .orderByDesc(UserProfilePO::getTotalScore);
        Page page = new Page().setCurrent(param.getPageNo()).setSize(param.getPageSize());
        return userProfilePOMapper.selectPage(page, queryWrapper);
    }

    /**
     * 查询需导出的积分记录
     *
     * @param param
     * @return
     */
    public List<AdminScoreRecordBO> getScoresList(AdminScoresPageDTO param) {
        LambdaQueryWrapper<UserProfilePO> queryWrapper =
                new LambdaQueryWrapper<UserProfilePO>()
                        .like(StringUtils.isNotEmpty(param.getName()), UserProfilePO::getName, param.getName())
                        .like(StringUtils.isNotEmpty(param.getJobNumber()), UserProfilePO::getJobNumber, param.getJobNumber())
                        .orderByDesc(UserProfilePO::getTotalScore);
        List<UserProfilePO> userProfilePOS = userProfilePOMapper.selectList(queryWrapper);
        return BeanConvertorUtils.copyList(userProfilePOS, AdminScoreRecordBO.class);
    }

    /**
     * 用户详情更新
     *
     * @param userProfilePO
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    public Integer updateSelection(UserProfilePO userProfilePO) {
        int updateSelection = userProfilePOMapper.updateSelection(userProfilePO);
        if (updateSelection == 0) {
            throw new BusinessException(BizResultCodeEnum.USER_PROFILE_UPDATE_ERROR.getMsg());
        }
        return updateSelection;
    }

    public Boolean updateSelfInformation(UserProfilePO userProfilePO) {
        LambdaUpdateWrapper<UserProfilePO> wrapper =
                new LambdaUpdateWrapper<UserProfilePO>()
                        .eq(UserProfilePO::getUserId, userProfilePO.getUserId())
                        .set(StringUtils.isNotEmpty(userProfilePO.getMobile()), UserProfilePO::getMobile, userProfilePO.getMobile())
                        .set(UserProfilePO::getPlateNo, userProfilePO.getPlateNo())
                        .set(UserProfilePO::getLivePlace, userProfilePO.getLivePlace());
        return this.update(wrapper);

    }

    /**
     * 根据用户列表获取所有的相应的列表
     *
     * @param userIdList
     * @return
     */
    public List<UserInfoPO> getUserInfoByUserIdList(Collection<Long> userIdList) {
        Map<Long, UserPO> userGroupByUserIdMap = userService.getUserByUserIdList(userIdList).stream()
                .collect(Collectors.toMap(UserPO::getUserId, userProfile -> userProfile));
        List<UserProfilePO> userProfilePOS = listByIds(userIdList);

        return userProfilePOS.stream().map(o -> {
            UserPO orDefault = userGroupByUserIdMap.getOrDefault(o.getUserId(), new UserPO());
            UserInfoPO userInfoPO = BeanConvertorUtils.map(o, UserInfoPO.class);
            userInfoPO.setOpenId(orDefault.getOpenId()).setState(orDefault.getState())
                    .setLastActiveAt(orDefault.getLastActiveAt());
            return userInfoPO;
        }).collect(Collectors.toList());

    }

    /**
     * 根据用户id获取用户详细信息
     *
     * @param userId
     * @return
     */
    public UserInfoPO getUserInfoByUserId(Long userId) {
        UserProfilePO userProfilePO = this.getById(userId);
        UserPO userPO = userService.getById(userId);
        UserInfoPO userInfoPO = BeanConvertorUtils.map(userProfilePO, UserInfoPO.class);
        userInfoPO.setOpenId(userPO.getOpenId()).setState(userPO.getState())
                .setLastActiveAt(userPO.getLastActiveAt());
        return userInfoPO;
    }

    /**
     * 批量查询用户信息
     *
     * @param userIdList
     * @return
     */
    public List<UserProfilePO> selectList(Collection<Long> userIdList) {
        LambdaQueryWrapper<UserProfilePO> queryWrapper = new LambdaQueryWrapper<UserProfilePO>().in(UserProfilePO::getUserId, userIdList);
        return userProfilePOMapper.selectList(queryWrapper);
    }

    /**
     * 用户详情批量更新
     *
     * @param userProfilePOS
     * @return
     */
    public int updateSelectionList(List<UserProfilePO> userProfilePOS) {
        return userProfilePOMapper.updateSelectionList(userProfilePOS);
    }

    /**
     * 减分校验，所扣分不能大于总分
     */
    public void checkTotalScoreList(Integer score, List<Long> userIdList) {
        if (score < 0) {
            LambdaQueryWrapper<UserProfilePO> queryWrapper = new LambdaQueryWrapper<UserProfilePO>()
                    .lt(UserProfilePO::getTotalScore, Math.abs(score)).in(UserProfilePO::getUserId, userIdList);
            List<UserProfilePO> userProfilePOList = userProfilePOMapper.selectList(queryWrapper);
            if (userProfilePOList.size() != 0) {
                String msg = "用户";
                for (UserProfilePO userProfilePO : userProfilePOList) {
                    msg += userProfilePO.getName() + "(" + userProfilePO.getJobNumber() + ") ";
                }
                throw new BusinessException(msg + "的总分不支持本次减分操作");
            }
        }
    }

    public List<UserProfilePO> queryBaseList() {
        return userProfilePOMapper.selectList(new LambdaQueryWrapper<>());
    }

    /**
     * 根据名称和工号查询userprofile
     * @param name
     * @param jobNumber
     * @return
     */
    public UserProfilePO selectUserProfileByNameAndJobNumber(String name, String jobNumber) {
        return userProfilePOMapper.selectOne(new LambdaQueryWrapper<UserProfilePO>()
                .eq(UserProfilePO::getName,name)
                .eq(UserProfilePO::getJobNumber,jobNumber));
    }
}
