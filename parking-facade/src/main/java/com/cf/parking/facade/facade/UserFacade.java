package com.cf.parking.facade.facade;


import com.cf.parking.facade.bo.AuthenticationBO;
import com.cf.parking.facade.bo.UserLoginBO;
import com.cf.parking.facade.bo.UserProfileBO;
import com.cf.parking.facade.dto.DriverEvaluateDTO;
import com.cf.parking.facade.dto.EvaluateDTO;
import com.cf.parking.facade.dto.UserProfileDTO;
import com.cf.support.result.Result;

public interface UserFacade {

    /**
     * 登录
     * @param code
     * @return
     */
    Result<UserLoginBO> loginByCode(String code);

    /**
     * 司机评价
     * @param driverEvaluateDTO
     * @return
     */
    Result driverEvaluate(DriverEvaluateDTO driverEvaluateDTO);

    /**
     * 获取他人信息
     * @param userId
     * @return
     */
    Result<UserProfileBO> otherProfile(Long userId);

    /**
     * 修改自己信息
     *
     * @param userProfileDTO
     * @return
     */
    Result changeProfile(UserProfileDTO userProfileDTO);

    /**
     * 乘客评价
     *
     * @param param
     * @return
     */
    Result passengerEvaluate(EvaluateDTO param);

    /**
     * 查询个人信息
     *
     * @param userId
     * @return
     */
    UserProfileBO getUserProfile(Long userId);

    /**
     *  获取jsapi鉴权信息
     * @return 鉴权信息
     */
    AuthenticationBO getAuthenticationInfo();
}
