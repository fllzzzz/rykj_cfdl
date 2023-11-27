package com.cf.parking.facade.facade;

import com.cf.parking.facade.bo.UserProfileBO;
import com.cf.parking.facade.bo.UserVerifyBO;
import com.cf.parking.facade.dto.UserVerifyDTO;
import com.cf.parking.facade.dto.UserVerifyOptDTO;
import com.cf.support.result.PageResponse;
import javax.servlet.http.HttpServletResponse;
import java.util.List;

/**
 * 车辆审核 Service接口
 * @author
 * @date 2023/9/7
 */
public interface UserVerifyFacade {

    /**
     * 查询车辆审核列表
     * @param dto
     * @return
     */
    PageResponse<UserVerifyBO> getPageUserVerifyList(UserVerifyDTO dto);

    /**
     * 获取摇车辆审核详细信息
     * @param dto
     * @return
     */
    UserVerifyBO getUserVerify(UserVerifyDTO dto);

    /**
     * 新增车辆审核
     * @param dto
     * @return
     */
    Integer add(UserVerifyOptDTO dto);

    /**
     * 审核车辆
     * @param dto
     * @return
     */
    Integer audit(UserVerifyOptDTO dto);

    /**
     * 批量审核车辆
     * @param dto
     * @return
     */
    void batchAudit(UserVerifyOptDTO dto);

    /**
     * 根据userId获取个人车牌号列表
     * @param userId
     * @return
     */
    List<String> getPlatNoListByUserId(Long userId);

    /**
     * 修改审核车辆
     * @param dto
     * @return
     */
    Integer update(UserVerifyOptDTO dto);

    /**
     * 删除个人车辆信息
     * @param id
     * @return
     */
    Integer deleteById(Long id);

    /**
     * 根据id查询车辆审核记录详细信息
     * @param id
     * @return
     */
    UserVerifyBO getUserVerifyInfoById(Long id);

    /**
     *车辆审核信息批量导出
     * @param boList
     * @return
     */
    void batchExport(List<UserVerifyBO> boList, HttpServletResponse response);


    /**
     * 根据条件查询所有符合条件的记录
     * @param dto
     * @return
     */
    List<UserVerifyBO> getAllUserVerifyList(UserVerifyDTO dto);

    /**
     * 根据车牌号查询车主信息
     * @param plateNo
     * @return
     */
    UserProfileBO getInfoByPlateNo(String plateNo);

    /**
     * 判断车牌号是否重复
     * @param plateNo
     * @return
     */
    Boolean judgePlateNoRepeat(String plateNo);

	/**
	 * 根据userid查询审核通过的车辆数量
	 * @param userId
	 * @return
	 */
	long queryAuditedCarCount(Long userId);
}
