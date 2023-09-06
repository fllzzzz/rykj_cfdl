package com.cf.parking.api.request;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 车位转赠记录
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
public class ParkingSpaceTransferRecordPageReq {

    /** id */
    private Long id;

    /** 转赠停车场id */
    private Long parkingLotId;

    /** 申请人userId */
    private Long userId;

    /** 赠予人userId */
    private Long acceptUserId;

    /** 赠予人车牌号 */
    private String acceptUserPlateNo;

    /** 转赠有效开始日期 */
    private Date validStartDate;

    /** 转赠有效截止日期 */
    private Date validEndDate;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
