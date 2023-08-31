package com.cf.parking.facade.facade;


import com.cf.parking.facade.bo.AdminScoreRecordBO;
import com.cf.parking.facade.bo.LoginBackBO;
import com.cf.parking.facade.dto.AdminScoreOptDTO;
import com.cf.parking.facade.dto.AdminScoresPageDTO;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;

/**
 * @author weihui
 * @date 2020/10/17
 */
public interface AdminUserFacade {
	Result<LoginBackBO> login(String emplNo, String password);

	Result logout(Long adminId);

	Result updatePwd(String emplNo, String oldPassword, String newPassword);

	/**
	 * 积分分页查询
	 *
	 * @param param
	 * @return
	 */
	PageResponse<AdminScoreRecordBO> getScoresPage(AdminScoresPageDTO param);

	/**
	 * 积分操作
	 *
	 * @param param
	 * @return
	 */
	Result scoreOpt(AdminScoreOptDTO param);

}
