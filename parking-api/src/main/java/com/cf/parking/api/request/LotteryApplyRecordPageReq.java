package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * 摇号申请记录
 * @author
 * @date 2023/09/05
 */
@Data
@Accessors(chain = true)
public class LotteryApplyRecordPageReq extends PageRequest {

    /** id */
    private Long id;

    /** 摇号批次id */
    private Long batchId;

    /** 用户 */
    private Long userId;

    /** 车牌号 */
    private String plateNo;

    /** 申请状态(0：取消申请；1：申请) */
    private String applyState;

    /** 摇号结果(-1：未开号；0：未中；xx：对应停车场的区域编号) */
    private String result;

    /** 创建时间 */
    private Date createTm;

    /** 更新时间 */
    private Date updateTm;
}
